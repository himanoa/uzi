{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Effectful.DiscordChannel.Interpreter where

import Control.Lens
import Data.Aeson
import Data.Coerce (coerce)
import Data.Discord.Channel
import Data.Discord.ChannelId
import Data.Discord.GuildId
import Data.Text (Text)
import Data.Text.Encoding
import Effectful
import Effectful.DiscordApiTokenReader (DiscordApiTokenReader, getToken)
import Effectful.DiscordChannel.Effect
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Internal.Monad
import Effectful.Req (Request, getResponseBodyAsJsonResponse, request)
import Network.HTTP.Req

host :: Text
host = "discord.com"

version :: Text
version = "v10"

runDiscordChannel :: (DiscordApiTokenReader :> es, Request :> es) => Eff (DiscordChannel : es) a -> Eff es a
runDiscordChannel = interpret $ \_ -> \case
  SendMessage params -> do
    token <- getToken
    _ <-
      request POST (https host /: "api" /: version /: "channels" /: coerce (params ^. channelId) /: "messages") (ReqBodyJson . toJSON $ params) ignoreResponse $
        header "Authorization" ("Bot " <> encodeUtf8 token)
    pure ()
  CreateChannel guildId params -> do
    token <- getToken
    _ <-
      request POST (https host /: "api" /: version /: "guilds" /: coerce guildId /: "channels") (ReqBodyJson . toJSON $ params) ignoreResponse $
        header "Authorization" ("Bot " <> encodeUtf8 token)
    pure ()
  GetChannels guildId -> do
    let pr = jsonResponse @[Channel]
    token <- getToken
    response <-
      request GET (https host /: "api" /: version /: "guilds" /: coerce guildId /: "channels") NoReqBody pr $
        header "Authorization" ("Bot " <> encodeUtf8 token)
    unsafeEff_ . getResponseBodyAsJsonResponse $ response
