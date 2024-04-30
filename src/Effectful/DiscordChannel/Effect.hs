{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- |
-- Module: Effectful.DiscordChannel.Effect
-- Description: DiscordChannelのAPIを実行するためのEffectです
-- Maintainer: himanoa <matsunoappy@gmail.com>
--
-- DiscordChannelに関するAPIを実行するためのEffectです。
--
-- Discord本家のChannelAPIの仕様はこちらです https://discord.com/developers/docs/resources/channel#channels-resource
module Effectful.DiscordChannel.Effect where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Discord hiding (channelId)
import Data.Discord.Channel (Channel, ChannelPosition)
import Data.Uzi.TimesChannel (TimesChannel)
import Effectful
import Effectful.Dispatch.Dynamic (HasCallStack, send)
import RIO hiding (HasCallStack, (^.))

-- | 投稿に対して使えるメンションのタイプの定義
data AllowedMentionTypes = Roles | Users | Everyone
  deriving (Show, Eq, Generic)
  deriving (ToJSON)

-- | 投稿に対して許可されているメンションの定義
data AllowedMention = AllowedMention
  { parse :: [AllowedMentionTypes],
    roles :: [Text],
    users :: [Text],
    repliedUser :: Bool,
    messageReferences :: Maybe MessageReferencesObject
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON)

-- | チャンネルにメッセージを投稿するためのデータ構造
data SendMessageParams = SendMessageParams
  { -- | 投稿先のチャンネルID
    _channelId :: ChannelId,
    -- | 投稿内容
    _content :: Content,
    _nonce :: Maybe Text,
    -- | 投稿内容がボイスチャンネルで音声読み上げされるか('True': 読み上げられる, 'False': 読み上げられない)
    _tts :: Bool,
    -- embeds is unsupported
    _allowedMentions :: Maybe AllowedMention,
    _messageReference :: Maybe MessageReferencesObject,
    _stickerIds :: Maybe Text
  }
  deriving (Show, Eq, Generic)

makeLenses ''SendMessageParams

-- | SendMessageのAPIリクエストに準拠している
instance ToJSON SendMessageParams where
  toJSON p = object ["channelId" .= (p ^. channelId), "content" .= (p ^. content), "nonce" .= (p ^. nonce), "tts" .= (p ^. tts), "allowedMentions" .= (p ^. allowedMentions), "messageReference" .= (p ^. messageReference), "stickerIds" .= (p ^. stickerIds)]

-- | 'Effectful.DiscordChannel.SendMessageParams' のスマートコンストラクタ
makeSendMessageParams :: ChannelId -> Content -> Maybe Text -> Bool -> Maybe AllowedMention -> Maybe MessageReferencesObject -> Maybe Text -> SendMessageParams
makeSendMessageParams = SendMessageParams

-- | 'Effectful.DiscordChannel.makeSendMessageParams' のラッパー
makeMessage ::
  -- | 投稿先のチャンネルID
  ChannelId ->
  -- | 投稿内容
  Content ->
  SendMessageParams
makeMessage cid con = do
  makeSendMessageParams cid con Nothing False Nothing Nothing Nothing

-- | チャンネルを作成するために必要なパラメータ
data CreateChannelParams = CreateChannelParams
  { -- | 作成したいチャンネル名
    __name :: ChannelName,
    -- | チャンネルの種類。[詳細](https://discord.com/developers/docs/resources/channel#channel-object-channel-types)
    __type :: Int
  }
  deriving (Show, Eq, Generic)

makeLenses ''CreateChannelParams

instance FromJSON CreateChannelParams where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 2}

instance ToJSON CreateChannelParams where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 2}

-- | 'Effectful.DiscordChannel.CreateChannelParams' を生成するためのスマートコンストラクタ
makeCreateChannelParams ::
  -- | 作成したいチャンネル名
  ChannelName ->
  CreateChannelParams
makeCreateChannelParams __name = CreateChannelParams {__name = __name, __type = 0}

data DiscordChannel :: Effect where
  SendMessage :: SendMessageParams -> DiscordChannel m ()
  CreateChannel :: GuildId -> CreateChannelParams -> DiscordChannel m ()
  GetChannels :: GuildId -> DiscordChannel m [Channel]
  -- FIXME: 手抜き実装でTimesに依存していて、他のChannelを変更する時に困るのでその時にリファクタリングする
  ModifyChannel :: GuildId -> ChannelId -> TimesChannel -> ChannelPosition -> DiscordChannel m ()

type instance DispatchOf DiscordChannel = Dynamic

-- | DiscordChannelにBotでテキストメッセージを送信します
sendMessage ::
  (HasCallStack, DiscordChannel :> es) =>
  SendMessageParams ->
  Eff es ()
sendMessage = send . SendMessage

-- | 新たにDiscordChannelを作成します
createChannel ::
  (HasCallStack, DiscordChannel :> es) =>
  -- | DiscordChannelを作成するサーバーのギルドID
  GuildId ->
  CreateChannelParams ->
  Eff es ()
createChannel guildId params = send (CreateChannel guildId params)

-- | 引数のギルドIDのサーバーに存在するチャンネルの一覧を取得します
getChannels ::
  (HasCallStack, DiscordChannel :> es) =>
  -- | 一覧を取得したいサーバーのギルドID
  GuildId ->
  -- | チャンネルの一覧
  Eff es [Channel]
getChannels guildId = send (GetChannels guildId)

-- | チャンネルの情報を更新します
modifyChannel ::
  (HasCallStack, DiscordChannel :> es) =>
  -- | 更新したいチャンネルが存在するDiscordサーバーのギルドId
  GuildId ->
  -- | チャンネル情報を更新したいチャンネルのID
  ChannelId ->
  -- | 更新後のChannelの情報
  TimesChannel ->
  -- | 更新後のチャンネルの順番
  ChannelPosition ->
  Eff es ()
modifyChannel guildId cId channel position = send (ModifyChannel guildId cId channel position)
