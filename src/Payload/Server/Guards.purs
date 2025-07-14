module Payload.Server.Guards
       ( headers
       , rawRequest
       , cookies

       , class ToGuardVal
       , toGuardVal

       , class RunGuards
       , runGuards
       ) where

import Prelude

import Control.Monad.Except (lift, throwError)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple)
import Effect.Aff (Aff)
import Foreign.Object as Object
import Node.HTTP as HTTP
import Node.HTTP.IncomingMessage as IM
import Payload.HTTP (HTTPRequest)
import Payload.Headers (Headers)
import Payload.Headers as Headers
import Payload.ResponseTypes (Failure(..), Response, Result)
import Payload.Server.Cookies as Cookies
import Payload.Server.Internal.GuardParsing (GuardTypes(..))
import Payload.Server.Response (class EncodeResponse)
import Payload.Server.Response as Resp
import Payload.Spec (GCons, GNil, Guards(..), GuardList)
import Prim.Row as Row
import Record as Record
import Type.Equality (to)
import Type.Proxy (Proxy(..))

-- | A guard function must return a value which can be converted
-- | to the type given in the guard spec.
-- | Guards can also fail and return a response directly, by returning
-- | Either.
class ToGuardVal a b where
  toGuardVal :: a -> Result b

instance toGuardValEitherFailureVal
  :: ToGuardVal (Either Failure a) a where
  toGuardVal (Left err) = throwError err
  toGuardVal (Right res) = pure res
else instance toGuardValEitherResponseVal ::
  EncodeResponse err
  => ToGuardVal (Either (Response err) a) a where
  toGuardVal (Left res) = do
    raw <- Resp.encodeResponse res
    throwError (Error raw) 
  toGuardVal (Right res) = pure res
else instance toGuardValEitherValVal ::
  EncodeResponse err
  => ToGuardVal (Either err a) a where
  toGuardVal (Left res) = do
    raw <- Resp.encodeResponse (Resp.internalError res)
    throwError (Error raw) 
  toGuardVal (Right res) = pure res
else instance toGuardValIdentity :: ToGuardVal a a where
  toGuardVal = pure

-- | Guard for retrieving request headers
headers :: HTTPRequest -> Aff Headers
headers req = pure (Headers.fromFoldable headersArr)
  where
    headersArr :: Array (Tuple String String)
    headersArr = Object.toUnfoldable $ IM.headers req

-- | Guard for retrieving raw underlying request
rawRequest :: HTTPRequest -> Aff HTTPRequest
rawRequest req = pure req

-- | Guard for retrieving request cookies
cookies :: HTTPRequest -> Aff (Map String String)
cookies req = pure (Cookies.requestCookies req)

type GuardFn r a = r -> Aff a

class RunGuards
  (guardNames :: GuardList)
  (guardsSpec :: Row Type)
  (allGuards :: Row Type)
  (results :: Row Type)
  (routeGuardSpec :: Row Type)
  r | guardNames guardsSpec allGuards -> routeGuardSpec where
  runGuards :: Guards guardNames
               -> GuardTypes (Record guardsSpec)
               -> Record allGuards
               -> Record results
               -> r
               -> Result (Record routeGuardSpec)

instance runGuardsNil :: RunGuards GNil guardsSpec allGuards routeGuardSpec routeGuardSpec r where
  runGuards _ _ allGuards results req = pure results

instance runGuardsCons ::
  ( IsSymbol name
  , Row.Cons name guardVal guardsSpec' guardsSpec
  , Row.Cons name (GuardFn r guardRes) allGuards' allGuards
  , Row.Cons name guardVal results newResults
  , Row.Lacks name results
  , ToGuardVal guardRes guardVal
  , RunGuards rest guardsSpec allGuards newResults routeGuardSpec r
  ) => RunGuards (GCons name rest) guardsSpec allGuards results routeGuardSpec r where
  runGuards _ _ allGuards results req = do
    let (guardHandler :: GuardFn r guardRes) = Record.get (Proxy :: Proxy name) (to allGuards)
    (guardHandlerResult :: guardRes) <- lift $ guardHandler req
    (guardResult :: guardVal) <- toGuardVal guardHandlerResult
    let newResults = Record.insert (Proxy :: Proxy name) guardResult results
    runGuards (Guards :: _ rest) (GuardTypes :: _ (Record guardsSpec)) allGuards newResults req
