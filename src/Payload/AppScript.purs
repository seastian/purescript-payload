module Payload.AppScript where

import Prelude

import Data.Either (Either(..))
import Data.List (fromFoldable)
import Data.Maybe (fromMaybe, maybe)
import Data.Nullable (Nullable, toMaybe)
import Data.String.Common (split)
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Console as Console
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import Foreign.Object (Object)
import Payload.ResponseTypes (Response(..), ResponseBody(..))
import Payload.RunHandlers (runHandlers)
import Payload.Server.Routable (class Routable, Outcome(..), mkRouter)
import Payload.Spec (Spec(..))

newtype Params = Params
  { parameters :: Object (Array String)
  , pathInfo :: Nullable String
  , contentLength :: Int
  , postData :: Nullable { contents :: String }
  }

foreign import appScriptJson :: String -> String

mkAppScriptHandler
  :: forall routesSpec handlers
   . Routable routesSpec {} handlers {} Params Effect
  => Spec routesSpec
  -> handlers
  -> EffectFn1 Params String
mkAppScriptHandler _ handlers = mkAppScriptHandlerGuarded api { handlers, guards: {} }
  where
    api = Spec :: Spec { routes :: routesSpec, guards :: {} }

mkAppScriptHandlerGuarded
  :: forall routesSpec guardsSpec handlers guards
   . Routable routesSpec guardsSpec handlers guards Params Effect
  => Spec { guards :: guardsSpec, routes :: routesSpec }
  -> { handlers :: handlers, guards :: guards }
  -> EffectFn1 Params String
mkAppScriptHandlerGuarded apiSpec api = mkEffectFn1 \p@(Params params) -> do
  let path = fromFoldable $ split (Pattern "/") $ fromMaybe "" $ toMaybe params.pathInfo
  let cfg = { logger: { log: Console.log, logDebug: Console.log, logError: Console.log } }
  let method = if params.contentLength == -1 then "GET" else "POST"
  let readBody _ = pure $ maybe "" _.contents $ toMaybe params.postData
  case mkRouter readBody apiSpec api of
    Right routerTrie -> do
        runHandlers cfg routerTrie { method, path, query: params.parameters } p >>= case _ of
            Success (Response { body: StringBody b }) -> pure $ appScriptJson b
            _ -> pure $ appScriptJson "ERROR"
    Left err -> pure $ appScriptJson $ "Router error: " <> show err
