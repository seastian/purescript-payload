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

import Node.HTTP.IncomingMessage (toReadable)
import Node.Stream.Aff (readAll, toStringUTF8)
import Payload.HTTP (HTTPRequest)
import Data.Array as Array
import Data.Either (Either(..))
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.String as String
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Console (log)
import Node.EventEmitter (on)
import Node.HTTP as HTTP
import Node.HTTP.IncomingMessage as HTTP.IncomingMessage
import Node.HTTP.Server (requestH, toNetServer)
import Node.HTTP.Server as HTTP.Server
import Node.HTTP.Types as HTTP.Types
import Node.Net.Server (listenTcp)
import Node.Net.Server as Net.Server
import Node.URL as Url
import Payload.HTTP (HTTPRequest, HTTPResponse)
import Payload.ResponseTypes (ResponseBody(..))
import Payload.RunHandlers (Config, Logger, runHandlers, showRouteUrl)
import Payload.Server.Internal.Querystring.Node (querystringParse)
import Payload.Server.Internal.Request (RequestUrl)
import Payload.Server.Internal.ServerResponse (sendResponse, writeResponse)
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
  , host :: String
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
  , host: "0.0.0.0"
  , port: 3000
  , logLevel: LogNormal }

newtype Server = Server HTTP.Types.HttpServer

-- | Start server with default options, ignoring unexpected startup errors.
launch
  :: forall routesSpec handlers
   . Routable routesSpec {} handlers {} HTTPRequest Aff
  => Spec routesSpec
  -> handlers
  -> Effect Unit
launch routeSpec handlers = Aff.launchAff_ (Aff.apathize $ start_ routeSpec handlers)

-- | Start server with default options and given route spec and handlers (no guards).
start_
  :: forall routesSpec handlers
   . Routable routesSpec {} handlers {} HTTPRequest Aff
  => Spec routesSpec
  -> handlers
  -> Aff (Either String Server)
start_ = start defaultOpts

-- | Start server with given routes and handlers (no guards).
start
  :: forall routesSpec handlers
   . Routable routesSpec {} handlers {} HTTPRequest Aff
  => Options
  -> Spec routesSpec
  -> handlers
  -> Aff (Either String Server)
start opts _ handlers = startGuarded opts api { handlers, guards: {} }
  where
    api = Spec :: Spec { routes :: routesSpec, guards :: {} }

-- | Start server with default options and given spec, handlers, and guards.
startGuarded_
  :: forall routesSpec guardsSpec handlers guards
   . Routable routesSpec guardsSpec handlers guards HTTPRequest Aff
  => Spec { routes :: routesSpec, guards :: guardsSpec }
  -> { handlers :: handlers, guards :: guards }
  -> Aff (Either String Server)
startGuarded_ = startGuarded defaultOpts

-- | Start server with given spec, handlers, and guards.
startGuarded
  :: forall routesSpec guardsSpec handlers guards
   . Routable routesSpec guardsSpec handlers guards HTTPRequest Aff
  => Options
  -> Spec { guards :: guardsSpec, routes :: routesSpec }
  -> { handlers :: handlers, guards :: guards }
  -> Aff (Either String Server)
startGuarded opts apiSpec api = do
  let cfg = mkConfig opts
  case mkRouter readBody apiSpec api of
    Right routerTrie -> do
      server <- liftEffect HTTP.createServer
      void $ liftEffect (server # on requestH \req res -> do
        Aff.launchAff_ $ handleRequest hostname cfg routerTrie req res)
      let httpOpts = Record.delete (Proxy :: Proxy "logLevel") $ Record.delete (Proxy :: Proxy "backlog") opts
      liftEffect $ listenTcp (toNetServer server) httpOpts
      cfg.logger.log startedMsg
      pure $ pure $ Server server
    Left err -> pure (Left err)
  where
  hostname = "http://" <> opts.host <> ":" <> show opts.port
  startedMsg = "Server is running on " <> hostname
  readBody req = toStringUTF8 =<< readAll (toReadable req)

dumpRoutes :: forall r m. Trie (HandlerEntry r m) -> Effect Unit
dumpRoutes = log <<< showRoutes

showRoutes :: forall r m. Trie (HandlerEntry r m)-> String
showRoutes routerTrie = Trie.dumpEntries (_.route <$> routerTrie)

mkConfig :: Options -> Config Aff
mkConfig { logLevel } = { logger: mkLogger logLevel }

mkLogger :: LogLevel -> Logger Aff
mkLogger logLevel = { log: log_, logDebug, logError }
  where
    log_ :: String -> Aff Unit
    log_ | logLevel >= LogNormal = log >>> liftEffect
    log_ = const $ pure unit

    logDebug :: String -> Aff Unit
    logDebug | logLevel >= LogDebug = log >>> liftEffect
    logDebug = const $ pure unit

    logError :: String -> Aff Unit
    logError | logLevel >= LogError = log >>> liftEffect
    logError = const $ pure unit

handleRequest :: String -> Config Aff -> Trie (HandlerEntry HTTPRequest Aff) -> HTTPRequest -> HTTPResponse -> Aff Unit
handleRequest hostname cfg@{ logger } routerTrie req res = do
  let url = HTTP.IncomingMessage.url req
  logger.logDebug (HTTP.IncomingMessage.method req <> " " <> url)
  liftEffect (requestUrl hostname req) >>= case _ of
    Right reqUrl -> runHandlers cfg routerTrie reqUrl req >>= case _ of
        Success r -> liftEffect $ sendResponse res r
        Failure r -> liftEffect $ sendResponse res r
        Forward _ -> liftEffect $ writeResponse res (Response.notFound (StringBody ""))
    Left err -> do
      liftEffect $ writeResponse res (internalError $ StringBody $ "Path could not be decoded: " <> show err)

showMatches :: forall r m. List (HandlerEntry r m) -> String
showMatches matches = "    " <> String.joinWith "\n    " (Array.fromFoldable $ showMatch <$> matches)
  where
    showMatch = showRouteUrl <<< _.route

requestUrl :: String -> HTTPRequest -> Effect (Either String RequestUrl)
requestUrl hostname req = do
  parsedUrl <- Url.new (hostname <> HTTP.IncomingMessage.url req)
  path <- Url.pathname parsedUrl
  query <- String.drop 1 >>> querystringParse <$> Url.search parsedUrl
  let pathSegments = urlToSegments path
  pure $ pure { method, path: pathSegments, query }
  where
    method = HTTP.IncomingMessage.method req

-- | Stops the server from accepting new connections and closes all connections connected to this server which are not sending a request or waiting for a response.
close :: Server -> Effect Unit
close (Server server) = Net.Server.close (toNetServer server)

-- | Closes all connections connected to this server.
closeAllConnections :: Server -> Effect Unit
closeAllConnections (Server server) = HTTP.Server.closeAllConnections server
