module Payload.Server.Guards.HTTP where

import Prelude

import Data.Map (Map)
import Data.Tuple (Tuple)
import Effect.Aff (Aff)
import Foreign.Object as Object
import Node.HTTP.IncomingMessage as IM
import Payload.HTTP (HTTPRequest)
import Payload.Headers (Headers)
import Payload.Headers as Headers
import Payload.Server.Cookies as Cookies

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
