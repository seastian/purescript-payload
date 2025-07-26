module Payload.Server.Guards
       ( class ToGuardVal
       , toGuardVal

       , class RunGuards
       , runGuards
       ) where

import Prelude

import Control.Monad.Except (lift, throwError)
import Data.Either (Either(..))
import Data.Symbol (class IsSymbol)
import Effect.Aff (Aff)
import Payload.ResponseTypes (Failure(..), Response, Result)
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
class ToGuardVal a b m where
  toGuardVal :: a -> Result b m

instance toGuardValEitherFailureVal
  :: Monad m => ToGuardVal (Either Failure a) a m where
  toGuardVal (Left err) = throwError err
  toGuardVal (Right res) = pure res
else instance toGuardValEitherResponseVal ::
  (EncodeResponse err m, Monad m)
  => ToGuardVal (Either (Response err) a) a m where
  toGuardVal (Left res) = do
    raw <- Resp.encodeResponse res
    throwError (Error raw) 
  toGuardVal (Right res) = pure res
else instance toGuardValEitherValVal ::
  (EncodeResponse err m, Monad m)
  => ToGuardVal (Either err a) a m where
  toGuardVal (Left res) = do
    raw <- Resp.encodeResponse (Resp.internalError res)
    throwError (Error raw) 
  toGuardVal (Right res) = pure res
else instance toGuardValIdentity :: Monad m => ToGuardVal a a m where
  toGuardVal = pure

type GuardFn :: forall k. Type -> k -> (k -> Type) -> Type
type GuardFn r a m = r -> m a

class RunGuards
  (guardNames :: GuardList)
  (guardsSpec :: Row Type)
  (allGuards :: Row Type)
  (results :: Row Type)
  (routeGuardSpec :: Row Type)
  r
  m | guardNames guardsSpec allGuards -> routeGuardSpec where
  runGuards :: Guards guardNames
               -> GuardTypes (Record guardsSpec)
               -> Record allGuards
               -> Record results
               -> r
               -> Result (Record routeGuardSpec) m

instance runGuardsNil :: Monad m => RunGuards GNil guardsSpec allGuards routeGuardSpec routeGuardSpec r m where
  runGuards _ _ allGuards results req = pure results

instance runGuardsCons ::
  ( IsSymbol name
  , Row.Cons name guardVal guardsSpec' guardsSpec
  , Row.Cons name (GuardFn r guardRes m) allGuards' allGuards
  , Row.Cons name guardVal results newResults
  , Row.Lacks name results
  , ToGuardVal guardRes guardVal m
  , RunGuards rest guardsSpec allGuards newResults routeGuardSpec r m
  , Monad m
  ) => RunGuards (GCons name rest) guardsSpec allGuards results routeGuardSpec r m where
  runGuards _ _ allGuards results req = do
    let (guardHandler :: GuardFn r guardRes m) = Record.get (Proxy :: Proxy name) (to allGuards)
    (guardHandlerResult :: guardRes) <- lift $ guardHandler req
    (guardResult :: guardVal) <- toGuardVal guardHandlerResult
    let newResults = Record.insert (Proxy :: Proxy name) guardResult results
    runGuards (Guards :: _ rest) (GuardTypes :: _ (Record guardsSpec)) allGuards newResults req
