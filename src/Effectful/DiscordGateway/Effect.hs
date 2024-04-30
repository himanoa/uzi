{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- |
-- Module: Effectful.DiscordGateway.Effect
-- Description: DiscordGatewayを実行するためのEffectの定義です。
-- Maintainer: himanoa <matsunoappy@gmail.com>
--
-- 'Effectful.DiscordChannel.Effect' を実行するインタプリタです。
--
-- 詳細: https://discord.com/developers/docs/topics/gateway
module Effectful.DiscordGateway.Effect where

import Data.Discord.Request
import Data.Discord.Response
import Effectful
import Effectful.Dispatch.Dynamic (HasCallStack, send)
import RIO hiding (HasCallStack)

data DiscordGateway :: Effect where
  ReceiveEvent :: DiscordGateway m (Maybe Response)
  SendEvent :: Request -> DiscordGateway m ()

type instance DispatchOf DiscordGateway = Dynamic

-- | WebSocket経由でDiscordから送信されてきたイベントをハンドリングします
receiveEvent :: (HasCallStack, DiscordGateway :> es) => Eff es (Maybe Response)
receiveEvent = send ReceiveEvent

-- | WebSocket経由でDiscordにイベントを送信します
sendEvent :: (HasCallStack, DiscordGateway :> es) => Request -> Eff es ()
sendEvent payload = send (SendEvent payload)
