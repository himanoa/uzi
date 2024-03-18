{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Effectful.DiscordChannel.Effect where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Discord hiding (channelId)
import Data.Text hiding (drop)
import Effectful
import Effectful.Dispatch.Dynamic (HasCallStack, send)
import GHC.Generics
import Data.Discord.Channel (Channel)

data AllowedMentionTypes = Roles | Users | Everyone
  deriving (Show, Eq, Generic)
  deriving (ToJSON)

data AllowedMention = AllowedMention
  { parse :: [AllowedMentionTypes],
    roles :: [Text],
    users :: [Text],
    repliedUser :: Bool,
    messageReferences :: Maybe MessageReferencesObject
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON)

data SendMessageParams = SendMessageParams
  { _channelId :: ChannelId,
    _content :: Content,
    _nonce :: Maybe Text,
    _tts :: Bool,
    -- embeds is unsupported
    _allowedMentions :: Maybe AllowedMention,
    _messageReference :: Maybe MessageReferencesObject,
    _stickerIds :: Maybe Text
  }
  deriving (Show, Eq, Generic)

makeLenses ''SendMessageParams

instance ToJSON SendMessageParams where
  toJSON p = object ["channelId" .= (p ^. channelId), "content" .= (p ^. content), "nonce" .= (p ^. nonce), "tts" .= (p ^. tts), "allowedMentions" .= (p ^. allowedMentions), "messageReference" .= (p ^. messageReference), "stickerIds" .= (p ^. stickerIds)]

makeSendMessageParams :: ChannelId -> Content -> Maybe Text -> Bool -> Maybe AllowedMention -> Maybe MessageReferencesObject -> Maybe Text -> SendMessageParams
makeSendMessageParams = SendMessageParams

makeMessage :: ChannelId -> Content -> SendMessageParams
makeMessage cid con = do
  makeSendMessageParams cid con Nothing False Nothing Nothing Nothing

data CreateChannelParams = CreateChannelParams
  { __name :: ChannelName,
    __type :: Int
  }
  deriving (Show, Eq, Generic)

makeLenses ''CreateChannelParams

instance FromJSON CreateChannelParams where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 2}

instance ToJSON CreateChannelParams where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 2}

makeCreateChannelParams :: ChannelName -> CreateChannelParams
makeCreateChannelParams __name = CreateChannelParams {__name = __name, __type = 0}

data DiscordChannel :: Effect where
  SendMessage :: SendMessageParams -> DiscordChannel m ()
  CreateChannel :: GuildId -> CreateChannelParams -> DiscordChannel m ()
  GetChannels :: GuildId -> DiscordChannel m [Channel]

type instance DispatchOf DiscordChannel = Dynamic

sendMessage :: (HasCallStack, DiscordChannel :> es) => SendMessageParams -> Eff es ()
sendMessage = send . SendMessage

createChannel :: (HasCallStack, DiscordChannel :> es) => GuildId -> CreateChannelParams -> Eff es ()
createChannel guildId params = send (CreateChannel guildId params)

getChannels :: (HasCallStack, DiscordChannel :> es) => GuildId -> Eff es [Channel]
getChannels guildId = send (GetChannels guildId)
