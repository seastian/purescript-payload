module Payload.HTTP where

import Node.HTTP.IncomingMessage as IncomingMessage
import Node.HTTP.Types as HTTP

type HTTPRequest = HTTP.IncomingMessage HTTP.IMServer

type HTTPResponse = HTTP.ServerResponse
