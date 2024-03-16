{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Effectful.DiscordChannel.Effect where

import Effectful
import Data.Discord
import Data.Text
import Effectful.Dispatch.Dynamic (HasCallStack, send)
import GHC.Generics
import Data.Aeson

data AllowedMentionTypes  = Roles | Users | Everyone
  deriving (Show,Eq, Generic)
  deriving ToJSON

data AllowedMention = AllowedMention {
  parse :: [AllowedMentionTypes],
  roles :: [Text],
  users :: [Text],
  repliedUser :: Bool,
  messageReferences :: Maybe MessageReferencesObject
}
  deriving (Show,Eq, Generic)
  deriving (ToJSON)

data SendMessageParams = SendMessageParams {
  channelId :: ChannelId,
  content :: Content,
  nonce :: Maybe Text,
  tts :: Bool,
  -- embeds is unsupported
  allowedMentions :: Maybe  AllowedMention,
  message_reference :: Maybe MessageReferencesObject,
  stickerIds :: Maybe Text
}
  deriving (Show,Eq, Generic)
  deriving ToJSON

data DiscordChannel :: Effect where 
  SendMessage :: SendMessageParams -> DiscordChannel m ()

type instance DispatchOf DiscordChannel = Dynamic

sendMessage :: (HasCallStack, DiscordChannel :> es) => SendMessageParams ->  Eff es ()
sendMessage = send . SendMessage
