module Payload.Examples.AppScript.Main where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn1)
import Payload.AppScript (Params, mkAppScriptHandler)
import Payload.Spec (Spec(Spec), GET)

type Message = 
  { id :: Int
  , text :: String }

spec :: Spec {
  getMessages :: GET "/users/<id>/messages?limit=<limit>" {
    params :: { id :: Int },
    query :: { limit :: Int },
    response :: Array Message
  }
}
spec = Spec

getMessages :: { params :: { id :: Int }, query :: { limit :: Int } } -> Effect (Array Message)
getMessages {params: {id}, query: {limit}} = pure
  [{ id: 1, text: "Hey " <> show id}, { id: 2, text: "Limit " <> show limit }]

handlers ::
    { getMessages ::
        { params :: { id :: Int }
        , query :: { limit :: Int }
        } -> Effect (Array { id :: Int, text :: String })
    }
handlers = { getMessages }

doGet :: EffectFn1 Params String
doGet = handler
  where
  handler = mkAppScriptHandler spec handlers
