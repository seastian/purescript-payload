module Payload.Server.Internal.Request where

import Data.List (List)
import Payload.Server.Internal.Querystring (ParsedQuery)

type RequestUrl =
  { method :: String
  , path :: List String
  , query :: ParsedQuery }
