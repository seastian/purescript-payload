module Payload.Server.Internal.ServerResponse where

import Prelude

import Data.Either (Either(..))
import Data.Traversable (sequence_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.Encoding as Encoding
import Node.HTTP.OutgoingMessage (setHeader, toWriteable)
import Node.HTTP.ServerResponse (setStatusCode, setStatusMessage, toOutgoingMessage)
import Node.Stream as Stream
import Payload.HTTP (HTTPResponse)
import Payload.Headers (Headers)
import Payload.Headers as Headers
import Payload.ResponseTypes (RawResponse, Response(..), ResponseBody(..), UnsafeStream)
import Payload.Server.Response (internalError)
import Type.Equality (to)
import Unsafe.Coerce (unsafeCoerce)

sendResponse :: HTTPResponse -> RawResponse -> Effect Unit
sendResponse res rawResp = Aff.runAff_ onComplete do
  liftEffect (writeResponse res rawResp)
  where
    onComplete (Left errors) = do
      log $ "Error sending response:\n  Server response:\n" <> show rawResp <>
        "\n\n  Error(s): " <> show errors
      writeResponse res (internalError (StringBody "Error sending server response"))
    onComplete (Right _) = pure unit

writeResponse :: HTTPResponse -> RawResponse -> Effect Unit
writeResponse res (Response serverRes) = do
  setStatusCode serverRes.status.code res
  setStatusMessage serverRes.status.reason res
  writeBodyAndHeaders res serverRes.headers serverRes.body

writeBodyAndHeaders :: HTTPResponse -> Headers -> ResponseBody -> Effect Unit
writeBodyAndHeaders res headers (StringBody str) = do
  let contentLength = show $ Encoding.byteLength str UTF8
  let newHeaders = Headers.setIfNotDefined "content-length" contentLength headers
  writeHeaders res newHeaders
  writeStringBody res str
writeBodyAndHeaders res headers (StreamBody stream) = do
  writeHeaders res headers
  writeStreamBody res stream
writeBodyAndHeaders res headers EmptyBody = do
  writeHeaders res headers
  Aff.launchAff_ $ endResponse res

foreign import endResponse_ :: HTTPResponse -> Unit -> (Unit -> Effect Unit) -> Effect Unit

endResponse :: HTTPResponse -> Aff Unit
endResponse res = Aff.makeAff \cb -> do
  endResponse_ res unit (\_ -> cb (Right unit))
  pure Aff.nonCanceler

writeHeaders :: HTTPResponse -> Headers -> Effect Unit
writeHeaders res headers = do
  let (sets :: Array (Effect Unit)) = map (\(Tuple k v) -> setHeader k v $ toOutgoingMessage res) (Headers.toUnfoldable headers)
  sequence_ sets

writeStringBody :: HTTPResponse -> String -> Effect Unit
writeStringBody res str = do
  let out = toWriteable $ toOutgoingMessage res
  -- Ignoring errors
  _ <- Stream.writeString' out UTF8 str (\_ -> pure unit)
  -- Ignoring errors
  Stream.end' out (\_ -> pure unit)

writeStreamBody :: HTTPResponse -> UnsafeStream -> Effect Unit
writeStreamBody res stream = do
  _ <- Stream.pipe (to (unsafeCoerce stream)) (toWriteable $ toOutgoingMessage res)
  pure unit
