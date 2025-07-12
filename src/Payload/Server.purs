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
import Data.List (List)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Nullable (toMaybe)
import Data.String as String
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (Error)
import Node.HTTP as HTTP
import Node.URL (URL)
import Node.URL as Url
import Payload.ResponseTypes (ResponseBody(..))
import Payload.RunHandlers (Config, Logger, runHandlers, showRouteUrl)
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

newtype Server = Server HTTP.Server

-- | Start server with default options, ignoring unexpected startup errors.
launch
  :: forall routesSpec handlers
   . Routable routesSpec {} handlers {} HTTP.Request
  => Spec routesSpec
  -> handlers
  -> Effect Unit
launch routeSpec handlers = Aff.launchAff_ (Aff.apathize $ start_ routeSpec handlers)

-- | Start server with default options and given route spec and handlers (no guards).
start_
  :: forall routesSpec handlers
   . Routable routesSpec {} handlers {} HTTP.Request
  => Spec routesSpec
  -> handlers
  -> Aff (Either String Server)
start_ = start defaultOpts

-- | Start server with given routes and handlers (no guards).
start
  :: forall routesSpec handlers
   . Routable routesSpec {} handlers {} HTTP.Request
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
   . Routable routesSpec guardsSpec handlers guards HTTP.Request
  => Spec { routes :: routesSpec, guards :: guardsSpec }
  -> { handlers :: handlers, guards :: guards }
  -> Aff (Either String Server)
startGuarded_ = startGuarded defaultOpts

-- | Start server with given spec, handlers, and guards.
startGuarded
  :: forall routesSpec guardsSpec handlers guards
   . Routable routesSpec guardsSpec handlers guards HTTP.Request
  => Options
  -> Spec { guards :: guardsSpec, routes :: routesSpec }
  -> { handlers :: handlers, guards :: guards }
  -> Aff (Either String Server)
startGuarded opts apiSpec api = do
  let cfg = mkConfig opts
  case mkRouter apiSpec api of
    Right routerTrie -> do
      server <- Server <$> (liftEffect $ HTTP.createServer (handleRequest cfg routerTrie))
      let httpOpts = Record.delete (Proxy :: Proxy "logLevel") opts
      listenResult <- listen cfg server httpOpts
      pure (const server <$> listenResult)
    Left err -> pure (Left err)

dumpRoutes :: forall r. Trie (HandlerEntry r) -> Effect Unit
dumpRoutes = log <<< showRoutes

showRoutes :: forall r. Trie (HandlerEntry r)-> String
showRoutes routerTrie = Trie.dumpEntries (_.route <$> routerTrie)

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

handleRequest :: Config -> Trie (HandlerEntry HTTP.Request) -> HTTP.Request -> HTTP.Response -> Effect Unit
handleRequest cfg@{ logger } routerTrie req res = do
  let url = Url.parse (HTTP.requestURL req)
  logger.logDebug (HTTP.requestMethod req <> " " <> show (url.path))
  case requestUrl req of
    Right reqUrl -> Aff.launchAff_ $ runHandlers cfg routerTrie reqUrl req >>= case _ of
        Success r -> liftEffect $ sendResponse res r
        Failure r -> liftEffect $ sendResponse res r
        Forward _ -> liftEffect $ writeResponse res (Response.notFound (StringBody ""))
    Left err -> do
      writeResponse res (internalError $ StringBody $ "Path could not be decoded: " <> show err)

showMatches :: forall r. List (HandlerEntry r) -> String
showMatches matches = "    " <> String.joinWith "\n    " (Array.fromFoldable $ showMatch <$> matches)
  where
    showMatch = showRouteUrl <<< _.route

requestUrl :: HTTP.Request -> Either String RequestUrl
requestUrl req = do
  let parsedUrl = Url.parse (HTTP.requestURL req)
  path <- urlPath parsedUrl
  let query = fromMaybe "" $ toMaybe parsedUrl.query
  let pathSegments = urlToSegments path
  pure { method, path: pathSegments, query }
  where
    method = HTTP.requestMethod req

urlPath :: URL -> Either String String
urlPath url = url.pathname
  # toMaybe
  # maybe (Left "No path") Right

urlQuery :: URL -> Maybe String
urlQuery url = url.query # toMaybe

foreign import onError :: HTTP.Server -> (Error -> Effect Unit) -> Effect Unit

listen :: Config -> Server -> HTTP.ListenOptions -> Aff (Either String Unit)
listen { logger } server@(Server httpServer) opts = Aff.makeAff $ \cb -> do
  onError httpServer \error -> cb (Right (Left (show error)))
  HTTP.listen httpServer opts (logger.log startedMsg *> cb (Right (Right unit)))
  pure $ Aff.Canceler (\error -> liftEffect (logger.logError (errorMsg error)) *> close server)
  where
    startedMsg = "Server is running on http://" <> opts.hostname <> ":" <> show opts.port
    errorMsg e = "Closing server due to error: " <> show e

-- | Stops the server from accepting new connections and closes all connections connected to this server which are not sending a request or waiting for a response.
close :: Server -> Aff Unit
close (Server server) = Aff.makeAff $ \cb -> do
  HTTP.close server (cb (Right unit))
  pure Aff.nonCanceler

foreign import closeAllConnectionsImpl :: HTTP.Server -> Effect Unit

-- | Closes all connections connected to this server.
closeAllConnections :: Server -> Effect Unit
closeAllConnections (Server server) = closeAllConnectionsImpl server
