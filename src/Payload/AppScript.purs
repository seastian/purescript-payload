module Payload.AppScript where

import Prelude

import Control.Promise (Promise, fromAff)
import Data.Either (Either(..))
import Data.Maybe (fromMaybe)
import Data.Nullable (Nullable, toMaybe)
import Effect.Console as Console
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import Foreign.Object (Object)
import Payload.ResponseTypes (Response(..), ResponseBody(..))
import Payload.RunHandlers (runHandlers)
import Payload.Server.Handleable (class RequestOps)
import Payload.Server.Routable (class Routable, Outcome(..), mkRouter)
import Payload.Spec (Spec(..))

newtype Params = Params
  { queryString :: Nullable String
  , parameter :: Object String
  , parameters :: Object (Array String)
  , pathInfo :: String
  }

instance RequestOps Params where
  readBody _ = pure ""

mkAppScriptHandler
  :: forall routesSpec handlers
   . Routable routesSpec {} handlers {} Params
  => Spec routesSpec
  -> handlers
  -> EffectFn1 Params (Promise String)
mkAppScriptHandler routeSpec handlers = mkAppScriptHandlerGuarded api { handlers, guards: {} }
  where
    api = Spec :: Spec { routes :: routesSpec, guards :: {} }

mkAppScriptHandlerGuarded
  :: forall routesSpec guardsSpec handlers guards
   . Routable routesSpec guardsSpec handlers guards Params
  => Spec { guards :: guardsSpec, routes :: routesSpec }
  -> { handlers :: handlers, guards :: guards }
  -> EffectFn1 Params (Promise String)
mkAppScriptHandlerGuarded apiSpec api = mkEffectFn1 \p@(Params params) -> do
  let cfg = { logger: { log: Console.log, logDebug: Console.log, logError: Console.log } }
  case mkRouter apiSpec api of
    Right routerTrie -> do
      fromAff do 
        runHandlers cfg routerTrie { method: "GET", path: pure params.pathInfo, query: fromMaybe "" $ toMaybe params.queryString } p >>= case _ of
            Success (Response { body: StringBody b }) -> pure b
            _ -> pure "ERROR"
    Left err -> fromAff do
        pure ""
