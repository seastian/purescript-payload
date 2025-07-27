module Payload.Server.Node.Stream where

import Prelude

import Data.Newtype (class Newtype)
import Node.Stream as Node
import Payload.ContentType as ContentType
import Payload.Headers as Headers
import Payload.ResponseTypes (Response(..), ResponseBody(..))
import Payload.Server.Response (class EncodeResponse)
import Unsafe.Coerce (unsafeCoerce)

newtype Stream r = Stream (Node.Stream r)

derive instance Newtype (Stream r) _

instance encodeResponseStream :: Monad m => EncodeResponse (Stream r) m where
  encodeResponse (Response r) = pure $ Response
                   { status: r.status
                   , headers: Headers.setIfNotDefined "content-type" ContentType.plain r.headers
                   , body: StreamBody (unsafeCoerce r.body) }
