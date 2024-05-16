{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- |
-- Module: Effectful.InteractionCallback.Effect
-- Description: UziBotのApplication Idを取得するEffect
-- Maintainer: himanoa <matsunoappy@gmail.com>
--
-- UziBotのApplication Idを読み出すためのEffect Moduleです
module Effectful.InteractionCallback.Effect
  ( InteractionCallback (..),
    doLoadingCallback,
    channelMessageCallback,
    channelMessageCallbackWithFlags,
  )
where

import Data.Discord.Content
import Data.Discord.MessageFlags
import Data.Discord.Response.InteractionCreateEventResponse
import Effectful
import Effectful.Dispatch.Dynamic (send)

-- | UziBotのApplication Idを読み出すためのEffectの定義
data InteractionCallback :: Effect where
  -- type: 4
  ChannelMessage :: InteractionCreateEventResponse -> Content -> MessageFlags -> InteractionCallback m ()
  -- type: 5
  Loading :: InteractionCreateEventResponse -> InteractionCallback m ()

type instance DispatchOf InteractionCallback = Dynamic

-- | ただちにチャンネルにメッセージを表示します。
channelMessageCallback :: (InteractionCallback :> es) => InteractionCreateEventResponse -> Content -> Eff es ()
channelMessageCallback _event _text = send (ChannelMessage _event _text (MessageFlags 9))

channelMessageCallbackWithFlags :: (InteractionCallback :> es) => InteractionCreateEventResponse -> Content -> MessageFlags -> Eff es ()
channelMessageCallbackWithFlags _event _text _flags = send (ChannelMessage _event _text _flags)

doLoadingCallback :: (InteractionCallback :> es) => InteractionCreateEventResponse -> Eff es ()
doLoadingCallback _event = send (Loading _event)
