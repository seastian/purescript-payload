module Payload.Server
       ( launch
       , start
       , start_
       , startGuarded
       , startGuarded_
       , Options
       , defaultOpts
       , LogLevel(..)
       , Server
       , close
       , closeAllConnections
       ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Nullable (toMaybe)
import Data.String as String
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (Error)
import Node.EventEmitter (on)
import Node.HTTP as HTTP
import Node.HTTP.IncomingMessage as HTTP.IncomingMessage
import Node.HTTP.Server as HTTP.Server
import Node.HTTP.Types as HTTP.Types
import Node.Net.Server (listenTcp)
import Node.Net.Server as NetServer
import Node.URL (URL)
import Node.URL as Url
import Payload.Internal.UrlParsing (Segment)
import Payload.ResponseTypes (ResponseBody(..))
import Payload.Server.Internal.Request (RequestUrl)
import Payload.Server.Internal.ServerResponse (writeResponse)
import Payload.Server.Internal.Trie (Trie)
import Payload.Server.Internal.Trie as Trie
import Payload.Server.Internal.UrlString (urlToSegments)
import Payload.Server.Response (internalError)
import Payload.Server.Response as Response
import Payload.Server.Routable (class Routable, HandlerEntry, Outcome(..), mkRouter)
import Payload.Spec (Spec(Spec))
import Record as Record
import Type.Proxy (Proxy(..))

type Options =
  { backlog :: Maybe Int
  , hostname :: String
  , port :: Int
  , logLevel :: LogLevel }

data LogLevel = LogSilent | LogError | LogNormal | LogDebug

instance eqLogLevel :: Eq LogLevel where
  eq LogSilent LogSilent = true
  eq LogError LogError = true
  eq LogNormal LogNormal = true
  eq LogDebug LogDebug = true
  eq _ _ = false

instance ordLogLevel :: Ord LogLevel where
  compare l1 l2 = rank l1 `compare` rank l2
    where
      rank :: LogLevel -> Int
      rank LogSilent = 0
      rank LogError = 1
      rank LogNormal = 2
      rank LogDebug = 3

defaultOpts :: Options
defaultOpts =
  { backlog: Nothing
  , hostname: "0.0.0.0"
  , port: 3000
  , logLevel: LogNormal }

newtype Server = Server HTTP.Types.HttpServer

type Config =
  { logger :: Logger }

type Logger =
  { log :: String -> Effect Unit
  , logDebug :: String -> Effect Unit
  , logError :: String -> Effect Unit
  }

-- | Start server with default options, ignoring unexpected startup errors.
launch
  :: forall routesSpec handlers
   . Routable routesSpec {} handlers {}
  => Spec routesSpec
  -> handlers
  -> Effect Unit
launch routeSpec handlers = Aff.launchAff_ (Aff.apathize $ start_ routeSpec handlers)

-- | Start server with default options and given route spec and handlers (no guards).
start_
  :: forall routesSpec handlers
   . Routable routesSpec {} handlers {}
  => Spec routesSpec
  -> handlers
  -> Aff (Either String Server)
start_ = start defaultOpts

-- | Start server with given routes and handlers (no guards).
start
  :: forall routesSpec handlers
   . Routable routesSpec {} handlers {}
  => Options
  -> Spec routesSpec
  -> handlers
  -> Aff (Either String Server)
start opts routeSpec handlers = startGuarded opts api { handlers, guards: {} }
  where
    api = Spec :: Spec { routes :: routesSpec, guards :: {} }

-- | Start server with default options and given spec, handlers, and guards.
startGuarded_
  :: forall routesSpec guardsSpec handlers guards
   . Routable routesSpec guardsSpec handlers guards
  => Spec { routes :: routesSpec, guards :: guardsSpec }
  -> { handlers :: handlers, guards :: guards }
  -> Aff (Either String Server)
startGuarded_ = startGuarded defaultOpts

-- | Start server with given spec, handlers, and guards.
startGuarded
  :: forall routesSpec guardsSpec handlers guards
   . Routable routesSpec guardsSpec handlers guards
  => Options
  -> Spec { guards :: guardsSpec, routes :: routesSpec }
  -> { handlers :: handlers, guards :: guards }
  -> Aff (Either String Server)
startGuarded opts apiSpec api = do
  let cfg = mkConfig opts
  case mkRouter apiSpec api of
    Right routerTrie -> do
      server@(Server httpServer) <- Server <$> (liftEffect $ HTTP.createServer)
      -- server <- Server <$> (liftEffect $ HTTP.createServer (handleRequest cfg routerTrie))
      void $ liftEffect $ httpServer # on HTTP.Server.requestH (handleRequest cfg routerTrie)
      listenResult <- listen cfg server opts
      pure (const server <$> listenResult)
    Left err -> pure (Left err)

mkConfig :: Options -> Config
mkConfig { logLevel } = { logger: mkLogger logLevel }

mkLogger :: LogLevel -> Logger
mkLogger logLevel = { log: log_, logDebug, logError }
  where
    log_ :: String -> Effect Unit
    log_ | logLevel >= LogNormal = log
    log_ = const $ pure unit

    logDebug :: String -> Effect Unit
    logDebug | logLevel >= LogDebug = log
    logDebug = const $ pure unit

    logError :: String -> Effect Unit
    logError | logLevel >= LogError = log
    logError = const $ pure unit

handleRequest :: Config -> Trie HandlerEntry -> HTTP.Types.IncomingMessage HTTP.Types.IMServer -> HTTP.Types.ServerResponse -> Effect Unit
handleRequest cfg@{ logger } routerTrie req res = do
  let url = HTTP.IncomingMessage.url req
  logger.logDebug (HTTP.IncomingMessage.method req <> " " <> show url)
  case requestUrl req of
    Right reqUrl -> runHandlers cfg routerTrie reqUrl req res
    Left err -> do
      writeResponse res (internalError $ StringBody $ "Path could not be decoded: " <> show err)

runHandlers :: Config -> Trie HandlerEntry -> RequestUrl
               -> HTTP.Types.IncomingMessage HTTP.Types.IMServer -> HTTP.Types.ServerResponse -> Effect Unit
runHandlers { logger } routerTrie reqUrl req res = do
  let (matches :: List HandlerEntry) = Trie.lookup (reqUrl.method : reqUrl.path) routerTrie
  let matchesStr = String.joinWith "\n" (Array.fromFoldable $ (showRouteUrl <<< _.route) <$> matches)
  logger.logDebug $ showUrl reqUrl <> " -> " <> show (List.length matches) <> " matches:\n" <> matchesStr
  Aff.launchAff_ $ do
    outcome <- handleNext Nothing matches
    case outcome of
      (Forward msg) -> do
        liftEffect $ writeResponse res (Response.notFound (StringBody ""))
      _ -> pure unit
  where
    handleNext :: Maybe Outcome -> List HandlerEntry -> Aff Outcome
    handleNext Nothing ({ handler } : rest) = do
      outcome <- handler reqUrl req res
      handleNext (Just outcome) rest
    handleNext (Just Success) _ = pure Success
    handleNext (Just Failure) _ = pure Failure
    handleNext (Just (Forward msg)) ({ handler } : rest) = do
      liftEffect $ logger.logDebug $ "-> Forwarding to next route. Previous failure: " <> msg
      outcome <- handler reqUrl req res
      handleNext (Just outcome) rest
    handleNext (Just (Forward msg)) Nil = do
      liftEffect $ logger.logDebug $ "-> No more routes to try. Last failure: " <> msg
      pure (Forward "No match could handle")
    handleNext _ Nil = pure (Forward "No match could handle")

showUrl :: RequestUrl -> String
showUrl { method, path } = method <> " " <> fullPath
  where fullPath = String.joinWith "/" (Array.fromFoldable path)

showRouteUrl :: List Segment -> String
showRouteUrl (method : rest) = show method <> " /" <> String.joinWith "/" (Array.fromFoldable $ show <$> rest)
showRouteUrl Nil = ""
  
requestUrl :: HTTP.Types.IncomingMessage HTTP.Types.IMServer -> Either String RequestUrl
requestUrl req = Left "asdfas"  -- do
  -- parsedUrl <- Url.new' $ HTTP.IncomingMessage.url req
  -- path <- urlPath parsedUrl
  -- let query = fromMaybe "" $ toMaybe parsedUrl.query
  -- let pathSegments = urlToSegments path
  -- pure { method, path: pathSegments, query }
  -- where
  --   method = HTTP.IncomingMessage.method req

foreign import onError :: HTTP.Types.HttpServer -> (Error -> Effect Unit) -> Effect Unit

listen :: Config -> Server -> Options -> Aff (Either String Unit)
listen { logger } server@(Server httpServer) opts = Aff.makeAff $ \cb -> do
  onError httpServer \error -> cb (Right (Left (show error)))
  -- HTTP.listen httpServer opts (logger.log startedMsg *> cb (Right (Right unit)))
  pure $ Aff.Canceler (\error -> liftEffect (logger.logError (errorMsg error)) *> close server)
  where
    startedMsg = "Server is running on http://" <> opts.hostname <> ":" <> show opts.port
    errorMsg e = "Closing server due to error: " <> show e

-- | Stops the server from accepting new connections and closes all connections connected to this server which are not sending a request or waiting for a response.
close :: Server -> Aff Unit
close (Server server) = liftEffect $ NetServer.close $ HTTP.Server.toNetServer server

-- | Closes all connections connected to this server.
closeAllConnections :: Server -> Effect Unit
closeAllConnections (Server server) = HTTP.Server.closeAllConnections server
