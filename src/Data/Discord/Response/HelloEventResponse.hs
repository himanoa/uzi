{-# LANGUAGE DerivingStrategies #-}
module Data.Discord.Response.HelloEventResponse (

) where

import GHC.Generics

--
-- ## やりたいこと
--
-- Decodeに成功したらJust 失敗したら Nothingを返す
--
-- ObjectはData.Aeson.Object
--
-- XXXResponseDecoder :: Object -> Maybe XXX
--
-- decoders = [
--   helloEventResponseDecoder,
--   XXXEventResponseDecoder,
--   ...
-- ]
--
-- data EventResponse = HelloEventResponse | XXXEventResponse deriving Show
--
-- decodeEventResponse :: List[Decoder] -> Maybe EventResponse
--
data HelloEventResponse = HelloEventResponse 
  deriving (Show, Generic) 
