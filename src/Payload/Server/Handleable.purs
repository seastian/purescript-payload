module Payload.Server.Handleable
       ( class Handleable
       , MethodHandler
       , handle
       , class DecodeOptionalBody
       , class RequestOps
       , readBody
       , decodeOptionalBody
       ) where

import Prelude

import Control.Monad.Except (except, lift, withExceptT)
import Data.Either (Either(..))
import Data.List (List)
import Data.Symbol (class IsSymbol)
import Effect.Aff (Aff)
import Node.HTTP.IncomingMessage (toReadable)
import Node.Stream.Aff (readAll, toStringUTF8)
import Payload.HTTP (HTTPRequest)
import Payload.Internal.Route (Undefined(..))
import Payload.Internal.UrlParsing (class ParseUrl, class ToSegments)
import Payload.ResponseTypes (Failure(Error, Forward), RawResponse, Response, ResponseBody(..), Result)
import Payload.Server.DecodeBody (class DecodeBody, decodeBody)
import Payload.Server.Guards (class RunGuards, runGuards)
import Payload.Server.Internal.GuardParsing (GuardTypes(..))
import Payload.Server.Internal.GuardParsing as GuardParsing
import Payload.Server.Internal.OmitEmpty (class OmitEmpty, omitEmpty)
import Payload.Server.Internal.Query as PayloadQuery
import Payload.Server.Internal.Request (RequestUrl)
import Payload.Server.Internal.Url as PayloadUrl
import Payload.Server.Response as Resp
import Payload.Spec (Guards(..), Route, GuardList)
import Prim.Row as Row
import Prim.Symbol as Symbol
import Type.Equality (class TypeEquals, to)
import Type.Proxy (Proxy(..))

type MethodHandler r = RequestUrl -> r -> Result RawResponse

class RequestOps r where
    readBody :: r -> Aff String

class Handleable
  route
  handler
  (basePath :: Symbol)
  (baseParams :: Row Type)
  (baseGuards :: GuardList)
  (guardsSpec :: Row Type)
  guards
  r | route -> handler where
  handle :: Proxy basePath
            -> Proxy (Record baseParams)
            -> Guards baseGuards
            -> GuardTypes (Record guardsSpec)
            -> route
            -> handler
            -> guards
            -> RequestUrl
            -> r
            -> Result RawResponse

instance handleablePostRoute ::
       ( TypeEquals (Record route)
           { response :: res
           , params :: Record params
           , query :: Record query
           , body :: body
           , guards :: Guards guardNames
           | r }
       , IsSymbol path
       , Symbol.Append "POST " fullPath docRoute
       , Resp.ToSpecResponse docRoute handlerRes res
       , Resp.EncodeResponse res
       , Symbol.Append basePath path fullPath
       , DecodeOptionalBody body

       , Row.Union baseParams params fullUrlParams
       , PayloadUrl.DecodeUrl fullPath fullUrlParams
       , PayloadQuery.DecodeQuery fullPath query
       , ParseUrl fullPath urlParts
       , ToSegments urlParts

       , TypeEquals
           { query :: Record query
           , params :: Record fullUrlParams
           , body :: body
           , guards :: Record routeGuardSpec }
           (Record payloadWithEmpty)
       , OmitEmpty payloadWithEmpty payload

       , GuardParsing.Append baseGuards guardNames fullGuards
       , RunGuards fullGuards guardsSpec allGuards () routeGuardSpec resp
       , RequestOps resp
       )
    => Handleable (Route "POST" path (Record route))
                  (Record payload -> Aff handlerRes)
                  basePath
                  baseParams
                  baseGuards
                  guardsSpec
                  (Record allGuards)
                  resp where
  handle _ _ _ _ route handler allGuards { method, path, query } req = do
    guards <- runGuards (Guards :: _ fullGuards) (GuardTypes :: _ (Record guardsSpec)) allGuards {} req
    params <- withExceptT Forward $ except $ decodePath path
    decodedQuery <- withExceptT badRequest $ except $ decodeQuery query
    bodyStr <- lift $ readBody req
    body <- withExceptT badRequest $ except $ (decodeOptionalBody bodyStr :: Either String body)
    let (payload :: Record payloadWithEmpty) = to { params, body, query: decodedQuery, guards: guards }
    mkResponse (Proxy :: _ docRoute) (Proxy :: _ res) (handler (omitEmpty payload))

    where
      badRequest :: String -> Failure
      badRequest _ = Error $ Resp.badRequest EmptyBody

      decodePath :: List String -> Either String (Record fullUrlParams)
      decodePath = PayloadUrl.decodeUrl (Proxy :: _ fullPath) (Proxy :: _ (Record fullUrlParams))

      decodeQuery :: String -> Either String (Record query)
      decodeQuery = PayloadQuery.decodeQuery (Proxy :: _ fullPath) (Proxy :: _ (Record query))

instance handleableGetRoute ::
       ( TypeEquals (Record route)
           { response :: res
           , params :: Record params
           , query :: Record query
           , guards :: Guards guardNames
           | r }
       , IsSymbol path
       , Symbol.Append "GET " fullPath docRoute
       , Resp.ToSpecResponse docRoute handlerRes res
       , Resp.EncodeResponse res
       , Symbol.Append basePath path fullPath

       , Row.Union baseParams params fullUrlParams
       , PayloadUrl.DecodeUrl fullPath fullUrlParams
       , PayloadQuery.DecodeQuery fullPath query
       , ParseUrl fullPath urlParts
       , ToSegments urlParts

       , TypeEquals
           { query :: Record query
           , params :: Record fullUrlParams
           , guards :: Record routeGuardSpec }
           (Record payloadWithEmpty)
       , OmitEmpty payloadWithEmpty payload

       , GuardParsing.Append baseGuards guardNames fullGuards
       , RunGuards fullGuards guardsSpec allGuards () routeGuardSpec resp
       , RequestOps resp
       )
    => Handleable (Route "GET" path (Record route))
                  (Record payload -> Aff handlerRes)
                  basePath
                  baseParams
                  baseGuards
                  guardsSpec
                  (Record allGuards)
                  resp where
  handle _ _ _ _ route handler allGuards { method, path, query } req = do
    guards <- runGuards (Guards :: _ fullGuards) (GuardTypes :: _ (Record guardsSpec)) allGuards {} req
    params <- withExceptT Forward $ except $ decodePath path
    decodedQuery <- withExceptT badRequest $ except $ decodeQuery query
    let (payload :: Record payloadWithEmpty) = to { params, query: decodedQuery, guards }
    mkResponse (Proxy :: _ docRoute) (Proxy :: _ res) (handler (omitEmpty payload))

    where
      badRequest :: String -> Failure
      badRequest _ = Error $ Resp.badRequest EmptyBody

      decodePath :: List String -> Either String (Record fullUrlParams)
      decodePath = PayloadUrl.decodeUrl (Proxy :: _ fullPath) (Proxy :: _ (Record fullUrlParams))

      decodeQuery :: String -> Either String (Record query)
      decodeQuery = PayloadQuery.decodeQuery (Proxy :: _ fullPath) (Proxy :: _ (Record query))

instance handleableHeadRoute ::
       ( TypeEquals (Record route)
           { response :: res
           , params :: Record params
           , query :: Record query
           , guards :: Guards guardNames
           | r }
       , IsSymbol path
       , Symbol.Append basePath path fullPath
       , Symbol.Append "HEAD " fullPath docRoute
       , Resp.ToSpecResponse docRoute handlerRes res
       , Resp.EncodeResponse res

       , Row.Union baseParams params fullUrlParams
       , PayloadUrl.DecodeUrl fullPath fullUrlParams
       , PayloadQuery.DecodeQuery fullPath query
       , ParseUrl fullPath urlParts
       , ToSegments urlParts

       , TypeEquals
           { query :: Record query
           , params :: Record fullUrlParams
           , guards :: Record routeGuardSpec }
           (Record payloadWithEmpty)
       , OmitEmpty payloadWithEmpty payload

       , GuardParsing.Append baseGuards guardNames fullGuards
       , RunGuards fullGuards guardsSpec allGuards () routeGuardSpec resp
       , RequestOps resp
       )
    => Handleable (Route "HEAD" path (Record route))
                  (Record payload -> Aff handlerRes)
                  basePath
                  baseParams
                  baseGuards
                  guardsSpec
                  (Record allGuards)
                  resp where
  handle _ _ _ _ route handler allGuards { method, path, query } req = do
    guards <- runGuards (Guards :: _ fullGuards) (GuardTypes :: _ (Record guardsSpec)) allGuards {} req
    params <- withExceptT Forward $ except $ decodePath path
    decodedQuery <- withExceptT badRequest $ except $ decodeQuery query
    let (payload :: Record payloadWithEmpty) = to { params, query: decodedQuery, guards }
    Resp.setBody EmptyBody <$> mkResponse (Proxy :: _ docRoute) (Proxy :: _ res) (handler (omitEmpty payload))

    where
      badRequest :: String -> Failure
      badRequest _ = Error $ Resp.badRequest EmptyBody

      decodePath :: List String -> Either String (Record fullUrlParams)
      decodePath = PayloadUrl.decodeUrl (Proxy :: _ fullPath) (Proxy :: _ (Record fullUrlParams))

      decodeQuery :: String -> Either String (Record query)
      decodeQuery = PayloadQuery.decodeQuery (Proxy :: _ fullPath) (Proxy :: _ (Record query))

instance handleablePutRoute ::
       ( TypeEquals (Record route)
           { response :: res
           , params :: Record params
           , query :: Record query
           , body :: body
           , guards :: Guards guardNames
           | r }
       , IsSymbol path
       , Symbol.Append "PUT " fullPath docRoute
       , Resp.ToSpecResponse docRoute handlerRes res
       , Resp.EncodeResponse res
       , Symbol.Append basePath path fullPath
       , DecodeOptionalBody body

       , Row.Union baseParams params fullUrlParams
       , PayloadUrl.DecodeUrl fullPath fullUrlParams
       , PayloadQuery.DecodeQuery fullPath query
       , ParseUrl fullPath urlParts
       , ToSegments urlParts

       , TypeEquals
           { query :: Record query
           , params :: Record fullUrlParams
           , body :: body
           , guards :: Record routeGuardSpec }
           (Record payloadWithEmpty)
       , OmitEmpty payloadWithEmpty payload

       , GuardParsing.Append baseGuards guardNames fullGuards
       , RunGuards fullGuards guardsSpec allGuards () routeGuardSpec resp
       , RequestOps resp
       )
    => Handleable (Route "PUT" path (Record route))
                  (Record payload -> Aff handlerRes)
                  basePath
                  baseParams
                  baseGuards
                  guardsSpec
                  (Record allGuards)
                  resp where
  handle _ _ _ _ route handler allGuards { method, path, query } req = do
    guards <- runGuards (Guards :: _ fullGuards) (GuardTypes :: _ (Record guardsSpec)) allGuards {} req
    params <- withExceptT Forward $ except $ decodePath path
    decodedQuery <- withExceptT badRequest $ except $ decodeQuery query
    bodyStr <- lift $ readBody req
    body <- withExceptT badRequest $ except $ (decodeOptionalBody bodyStr :: Either String body)
    let (payload :: Record payloadWithEmpty) = to { params, body, query: decodedQuery, guards }
    mkResponse (Proxy :: _ docRoute) (Proxy :: _ res) (handler (omitEmpty payload))

    where
      badRequest :: String -> Failure
      badRequest _ = Error $ Resp.badRequest EmptyBody

      decodePath :: List String -> Either String (Record fullUrlParams)
      decodePath = PayloadUrl.decodeUrl (Proxy :: _ fullPath) (Proxy :: _ (Record fullUrlParams))

      decodeQuery :: String -> Either String (Record query)
      decodeQuery = PayloadQuery.decodeQuery (Proxy :: _ fullPath) (Proxy :: _ (Record query))

instance handleableDeleteRoute ::
       ( TypeEquals (Record route)
           { response :: res
           , params :: Record params
           , query :: Record query
           , body :: body
           , guards :: Guards guardNames
           | r }
       , IsSymbol path
       , Symbol.Append "DELETE " fullPath docRoute
       , Resp.ToSpecResponse docRoute handlerRes res
       , Resp.EncodeResponse res
       , Symbol.Append basePath path fullPath
       , DecodeOptionalBody body

       , Row.Union baseParams params fullUrlParams
       , PayloadUrl.DecodeUrl fullPath fullUrlParams
       , PayloadQuery.DecodeQuery fullPath query
       , ParseUrl fullPath urlParts
       , ToSegments urlParts

       , TypeEquals
           { query :: Record query
           , params :: Record fullUrlParams
           , body :: body
           , guards :: Record routeGuardSpec }
           (Record payloadWithEmpty)
       , OmitEmpty payloadWithEmpty payload

       , GuardParsing.Append baseGuards guardNames fullGuards
       , RunGuards fullGuards guardsSpec allGuards () routeGuardSpec resp
       , RequestOps resp
       )
    => Handleable (Route "DELETE" path (Record route))
                  (Record payload -> Aff handlerRes)
                  basePath
                  baseParams
                  baseGuards
                  guardsSpec
                  (Record allGuards)
                  resp where
  handle _ _ _ _ route handler allGuards { method, path, query } req = do
    guards <- runGuards (Guards :: _ fullGuards) (GuardTypes :: _ (Record guardsSpec)) allGuards {} req
    params <- withExceptT Forward $ except $ decodePath path
    decodedQuery <- withExceptT badRequest $ except $ decodeQuery query
    bodyStr <- lift $ readBody req
    body <- withExceptT badRequest $ except $ (decodeOptionalBody bodyStr :: Either String body)
    let (payload :: Record payloadWithEmpty) = to { params, body, query: decodedQuery, guards }
    mkResponse (Proxy :: _ docRoute) (Proxy :: _ res) (handler (omitEmpty payload))

    where
      badRequest :: String -> Failure
      badRequest _ = Error $ Resp.badRequest EmptyBody

      decodePath :: List String -> Either String (Record fullUrlParams)
      decodePath = PayloadUrl.decodeUrl (Proxy :: _ fullPath) (Proxy :: _ (Record fullUrlParams))

      decodeQuery :: String -> Either String (Record query)
      decodeQuery = PayloadQuery.decodeQuery (Proxy :: _ fullPath) (Proxy :: _ (Record query))

instance handleableOptionsRoute ::
       ( TypeEquals (Record route)
           { response :: res
           , params :: Record params
           , query :: Record query
           , guards :: Guards guardNames
           | r }
       , IsSymbol path
       , Symbol.Append "OPTIONS " fullPath docRoute
       , Resp.ToSpecResponse docRoute handlerRes res
       , Resp.EncodeResponse res
       , Symbol.Append basePath path fullPath

       , Row.Union baseParams params fullUrlParams
       , PayloadUrl.DecodeUrl fullPath fullUrlParams
       , PayloadQuery.DecodeQuery fullPath query
       , ParseUrl fullPath urlParts
       , ToSegments urlParts

       , TypeEquals
           { query :: Record query
           , params :: Record fullUrlParams
           , guards :: Record routeGuardSpec }
           (Record payloadWithEmpty)
       , OmitEmpty payloadWithEmpty payload

       , GuardParsing.Append baseGuards guardNames fullGuards
       , RunGuards fullGuards guardsSpec allGuards () routeGuardSpec resp
       , RequestOps resp
       )
    => Handleable (Route "OPTIONS" path (Record route))
                  (Record payload -> Aff handlerRes)
                  basePath
                  baseParams
                  baseGuards
                  guardsSpec
                  (Record allGuards)
                  resp where
  handle _ _ _ _ route handler allGuards { method, path, query } req = do
    guards <- runGuards (Guards :: _ fullGuards) (GuardTypes :: _ (Record guardsSpec)) allGuards {} req
    params <- withExceptT Forward $ except $ decodePath path
    decodedQuery <- withExceptT badRequest $ except $ decodeQuery query
    let (payload :: Record payloadWithEmpty) = to { params, query: decodedQuery, guards }
    mkResponse (Proxy :: _ docRoute) (Proxy :: _ res) (handler (omitEmpty payload))

    where
      badRequest :: String -> Failure
      badRequest _ = Error $ Resp.badRequest EmptyBody

      decodePath :: List String -> Either String (Record fullUrlParams)
      decodePath = PayloadUrl.decodeUrl (Proxy :: _ fullPath) (Proxy :: _ (Record fullUrlParams))

      decodeQuery :: String -> Either String (Record query)
      decodeQuery = PayloadQuery.decodeQuery (Proxy :: _ fullPath) (Proxy :: _ (Record query))

mkResponse :: forall handlerRes res docRoute
  . Resp.ToSpecResponse docRoute handlerRes res
  => Resp.EncodeResponse res
  => Proxy docRoute -> Proxy res -> Aff handlerRes -> Result RawResponse
mkResponse _ _ aff = do
  (handlerResp :: handlerRes) <- lift $ aff
  (specResp :: Response res) <- Resp.toSpecResponse (Proxy :: _ docRoute) handlerResp
  (rawResp :: RawResponse) <- Resp.encodeResponse specResp
  pure rawResp

instance RequestOps HTTPRequest where
  readBody req = toStringUTF8 =<< readAll (toReadable req)

class DecodeOptionalBody
  (body :: Type)
  where
    decodeOptionalBody :: String -> Either String body

instance decodeOptionalBodyUndefined :: DecodeOptionalBody Undefined where
  decodeOptionalBody _ = Right Undefined
else instance encodeOptionalBodyDefined ::
  DecodeBody body => DecodeOptionalBody body where
  decodeOptionalBody body = decodeBody body
