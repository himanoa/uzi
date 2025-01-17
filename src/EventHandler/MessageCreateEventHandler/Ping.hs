{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- |
-- Module: EventHandler.MessageCreateEventHandler.Ping
-- Description: Discordメッセージ作成イベントの"ping"に応答するハンドラです。
-- Maintainer: himanoa <matsunoappy@gmail.com>
module EventHandler.MessageCreateEventHandler.Ping where

import Control.Lens
import Data.Discord
import Data.Discord.Response.InteractionCreateEventResponse qualified as IC
import Data.Either.Validation
import Effectful
import Effectful.DynamicLogger
import Effectful.InteractionCallback
import Effectful.NonDet
import RIO hiding ((^.))

-- | "ping"メッセージに応答して"pong"を返します。
pingEventHandler :: (NonDet :> es, DynamicLogger :> es, InteractionCallback :> es) => Response -> Eff es ()
pingEventHandler = \case
  InteractionCreate event -> do
    if (event ^. IC.slashCommandName) == "ping"
      then case makeContent "pong" of
        Success c -> do
          info "Dispatched Ping Handler"
          channelMessageCallback event c
          pure ()
        Failure _ -> pure ()
      else emptyEff
    pure ()
  _ -> emptyEff
