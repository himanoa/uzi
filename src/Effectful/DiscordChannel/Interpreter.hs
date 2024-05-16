{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- |
-- Module: Effectful.DiscordChannel.Interpreter
-- Description: 'Effectful.DiscordChannel.Effect' を実行するインタプリタです。
-- Maintainer: himanoa <matsunoappy@gmail.com>
--
-- 'Effectful.DiscordChannel.Effect' を実行するインタプリタです。
module Effectful.DiscordChannel.Interpreter where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Coerce (coerce)
import Data.Discord.Channel qualified as C
import Data.Discord.ChannelId
import Data.Discord.GuildId
import Effectful
import Effectful.DiscordApiTokenReader (DiscordApiTokenReader, getToken)
import Effectful.DiscordChannel.Effect
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Internal.Monad
import Effectful.Req (Request, getResponseBodyAsJsonResponse, request)
import Network.HTTP.Req
import RIO hiding ((^.))

-- | DiscordAPIのホスト部分を返す
host :: Text
host = "discord.com"

-- | DiscordAPIのversionを返す
version :: Text
version = "v10"

-- | DiscordChannelAPIを実行します
runDiscordChannel :: (DiscordApiTokenReader :> es, Request :> es) => Eff (DiscordChannel : es) a -> Eff es a
runDiscordChannel = interpret $ \_ -> \case
  SendMessage params -> do
    token <- getToken
    _ <-
      request POST (https host /: "api" /: version /: "channels" /: coerce (params ^. channelId) /: "messages") (ReqBodyJson . toJSON $ params) ignoreResponse
        $ header "Authorization" ("Bot " <> encodeUtf8 token)
    pure ()
  CreateChannel guildId params -> do
    token <- getToken
    _ <-
      request POST (https host /: "api" /: version /: "guilds" /: coerce guildId /: "channels") (ReqBodyJson . toJSON $ params) ignoreResponse
        $ header "Authorization" ("Bot " <> encodeUtf8 token)
    pure ()
  GetChannels guildId -> do
    let pr = jsonResponse @[C.Channel]
    token <- getToken
    response <-
      request GET (https host /: "api" /: version /: "guilds" /: coerce guildId /: "channels") NoReqBody pr
        $ header "Authorization" ("Bot " <> encodeUtf8 token)
    unsafeEff_ . getResponseBodyAsJsonResponse $ response
  ModifyChannel _ parentId channel pos -> do
    token <- getToken
    let payload = object ["type" .= (0 :: Integer), "position" .= C.coerceChannelPosition pos, "parent_id" .= coerceChannelId parentId]
    _ <-
      request PATCH (https host /: "api" /: version /: "channels" /: coerceChannelId (channel ^. C._id)) (ReqBodyJson payload) ignoreResponse
        $ header "Authorization" ("Bot " <> encodeUtf8 token)
    pure ()
