module Payload.RunHandlers where

import Prelude

import Data.Array as Array
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.String.Common as String
import Payload.Internal.UrlParsing (Segment)
import Payload.Server.Internal.Request (RequestUrl)
import Payload.Server.Internal.Trie (Trie)
import Payload.Server.Internal.Trie as Trie
import Payload.Server.Routable (HandlerEntry, Outcome(..))

type Config m =
  { logger :: Logger m }

type Logger m =
  { log :: String -> m Unit
  , logDebug :: String -> m Unit
  , logError :: String -> m Unit
  }

showRouteUrl :: List Segment -> String
showRouteUrl (method : rest) = show method <> " /" <> String.joinWith "/" (Array.fromFoldable $ show <$> rest)
showRouteUrl Nil = ""

showUrl :: RequestUrl -> String
showUrl { method, path, query } = method <> " " <> fullPath
  where fullPath = String.joinWith "/" (Array.fromFoldable path)

runHandlers :: forall r m. Monad m => Config m -> Trie (HandlerEntry r m) -> RequestUrl
               -> r -> m Outcome
runHandlers { logger } routerTrie reqUrl req = do
  let (matches :: List (HandlerEntry r m)) = Trie.lookup (reqUrl.method : reqUrl.path) routerTrie
  let matchesStr = String.joinWith "\n" (Array.fromFoldable $ (showRouteUrl <<< _.route) <$> matches)
  logger.logDebug $ showUrl reqUrl <> " -> " <> show (List.length matches) <> " matches:\n" <> matchesStr
  handleNext Nothing matches
  where
    handleNext :: Maybe Outcome -> List (HandlerEntry r m) -> m Outcome
    handleNext Nothing ({ handler } : rest) = do
      outcome <- handler reqUrl req
      handleNext (Just outcome) rest
    handleNext (Just (Success r)) _ = pure $ Success r
    handleNext (Just (Failure r)) _ = pure $ Failure r
    handleNext (Just (Forward msg)) ({ handler } : rest) = do
      logger.logDebug $ "-> Forwarding to next route. Previous failure: " <> msg
      outcome <- handler reqUrl req
      handleNext (Just outcome) rest
    handleNext (Just (Forward msg)) Nil = do
      logger.logDebug $ "-> No more routes to try. Last failure: " <> msg
      pure (Forward "No match could handle")
    handleNext _ Nil = pure (Forward "No match could handle")

