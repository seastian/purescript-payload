module Payload.Server.ReadBody where

import Effect.Aff (Aff)

type ReadBody r = r -> Aff String
