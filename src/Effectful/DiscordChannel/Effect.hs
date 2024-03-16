{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Effectful.DiscordChannel.Effect where

import Effectful
import Data.Discord hiding (channelId)
import Data.Text
import Effectful.Dispatch.Dynamic (HasCallStack, send)
import GHC.Generics
import Data.Aeson
import Control.Lens hiding ((.=))

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
  _channelId :: ChannelId,
  _content :: Content,
  _nonce :: Maybe Text,
  _tts :: Bool,
  -- embeds is unsupported
  _allowedMentions :: Maybe  AllowedMention,
  _messageReference :: Maybe MessageReferencesObject,
  _stickerIds :: Maybe Text
}
  deriving (Show,Eq, Generic)

makeLenses ''SendMessageParams

instance ToJSON SendMessageParams where
  toJSON p = object ["channelId" .= (p ^. channelId), "content" .= (p ^. content), "nonce" .= (p^. nonce), "tts" .= (p ^. tts), "allowedMentions" .= (p ^. allowedMentions), "messageReference" .= (p ^. messageReference), "stickerIds" .= (p ^. stickerIds)]


makeSendMessageParams :: ChannelId -> Content -> Maybe Text -> Bool -> Maybe AllowedMention -> Maybe MessageReferencesObject -> Maybe Text -> SendMessageParams
makeSendMessageParams = SendMessageParams

data DiscordChannel :: Effect where 
  SendMessage :: SendMessageParams -> DiscordChannel m ()

type instance DispatchOf DiscordChannel = Dynamic

sendMessage :: (HasCallStack, DiscordChannel :> es) => SendMessageParams ->  Eff es ()
sendMessage = send . SendMessage
