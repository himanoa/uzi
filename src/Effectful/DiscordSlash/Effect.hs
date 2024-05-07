{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- |
-- Module: Effectful.スラッシュコマンド.Effect
-- Description: スラッシュコマンドのAPIを実行するためのEffectです
-- Maintainer: himanoa <matsunoappy@gmail.com>
--
-- スラッシュコマンドに関するAPIを実行するためのEffectです。
--
-- Discord本家のスラッシュコマンドの仕様はこちらです https://discord.com/developers/docs/interactions/application-commands#
module Effectful.DiscordSlash.Effect
  ( SlashCommand (..),
    makeGlobalSlashCommand,
  )
where

import Data.Discord.Request.SlashCommand (CommandOption, Description (Description), Name (Name))
import Effectful
import RIO hiding (HasCallStack, (^.))

data SlashCommand :: Effect where
  GlobalCommand :: Name -> Description -> [CommandOption] -> SlashCommand m ()

type instance DispatchOf SlashCommand = Dynamic

-- | 'Effectful.DiscordSlash.SendMessageParams' のスマートコンストラクタ
makeGlobalSlashCommand :: Text -> Text -> [CommandOption] -> SlashCommand m ()
makeGlobalSlashCommand name desc = GlobalCommand (Name name) (Description desc)
