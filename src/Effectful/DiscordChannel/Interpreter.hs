{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Effectful.DiscordChannel.Interpreter where

import Effectful.DiscordApiTokenReader (DiscordApiTokenReader)
import Effectful
import Effectful.Req (Request, request)
import Effectful.DiscordChannel.Effect
import Effectful.Dispatch.Dynamic (interpret)
import Network.HTTP.Req (POST(POST), https, ReqBodyJson (ReqBodyJson), ignoreResponse)
import Data.Text (Text)
import Data.Aeson
import Data.String.Conversions

host :: Text
host = "https://discord.com/api"

runDiscordChannel :: (DiscordApiTokenReader :> es, Request :> es) => Eff (DiscordChannel : es) a -> Eff es a
runDiscordChannel = interpret $ \_ ->  \case
  SendMessage params  -> do
    let endpoint = host <> "/channel/" <> (convertString . show $ params.channelId) <> "/messages"
    _ <- request POST (https endpoint) (ReqBodyJson . toJSON $ params) ignoreResponse mempty
    pure ()
