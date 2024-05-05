{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
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
module Effectful.DiscordSlash.Effect where

import Data.Discord.Request.SlashCommand (CommandOption, Description (Description), Name (Name))
import Effectful
import RIO hiding (HasCallStack, (^.))

data SlashCommand :: Effect where
  GlobalCommand :: Name -> Description -> [CommandOption] -> SlashCommand m ()

type instance DispatchOf SlashCommand = Dynamic

-- | 'Effectful.DiscordChannel.SendMessageParams' のスマートコンストラクタ
makeGlobalSlashCommand :: Text -> Text -> [CommandOption] -> SlashCommand m ()
makeGlobalSlashCommand name desc = GlobalCommand (Name name) (Description desc)
