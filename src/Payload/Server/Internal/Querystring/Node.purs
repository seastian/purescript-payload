module Payload.Server.Internal.Querystring.Node where

import Foreign.Object (Object)

foreign import querystringParse :: String -> Object (Array String)
