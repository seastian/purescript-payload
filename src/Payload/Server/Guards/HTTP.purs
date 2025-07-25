module Payload.Server.Guards.HTTP where

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
