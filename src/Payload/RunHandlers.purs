module Payload.RunHandlers where

import Prelude

import Data.Array as Array
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.String.Common as String
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Payload.Internal.UrlParsing (Segment)
import Payload.Server.Internal.Request (RequestUrl)
import Payload.Server.Internal.Trie (Trie)
import Payload.Server.Internal.Trie as Trie
import Payload.Server.Routable (HandlerEntry, Outcome(..))

type Config =
  { logger :: Logger }

type Logger =
  { log :: String -> Effect Unit
  , logDebug :: String -> Effect Unit
  , logError :: String -> Effect Unit
  }

showRouteUrl :: List Segment -> String
showRouteUrl (method : rest) = show method <> " /" <> String.joinWith "/" (Array.fromFoldable $ show <$> rest)
showRouteUrl Nil = ""

showUrl :: RequestUrl -> String
showUrl { method, path, query } = method <> " " <> fullPath
  where fullPath = String.joinWith "/" (Array.fromFoldable path)

runHandlers :: forall r. Config -> Trie (HandlerEntry r) -> RequestUrl
               -> r -> Aff Outcome
runHandlers { logger } routerTrie reqUrl req = do
  let (matches :: List (HandlerEntry r)) = Trie.lookup (reqUrl.method : reqUrl.path) routerTrie
  let matchesStr = String.joinWith "\n" (Array.fromFoldable $ (showRouteUrl <<< _.route) <$> matches)
  liftEffect $ logger.logDebug $ showUrl reqUrl <> " -> " <> show (List.length matches) <> " matches:\n" <> matchesStr
  handleNext Nothing matches
  where
    handleNext :: Maybe Outcome -> List (HandlerEntry r) -> Aff Outcome
    handleNext Nothing ({ handler } : rest) = do
      outcome <- handler reqUrl req
      handleNext (Just outcome) rest
    handleNext (Just (Success r)) _ = pure $ Success r
    handleNext (Just (Failure r)) _ = pure $ Failure r
    handleNext (Just (Forward msg)) ({ handler } : rest) = do
      liftEffect $ logger.logDebug $ "-> Forwarding to next route. Previous failure: " <> msg
      outcome <- handler reqUrl req
      handleNext (Just outcome) rest
    handleNext (Just (Forward msg)) Nil = do
      liftEffect $ logger.logDebug $ "-> No more routes to try. Last failure: " <> msg
      pure (Forward "No match could handle")
    handleNext _ Nil = pure (Forward "No match could handle")

