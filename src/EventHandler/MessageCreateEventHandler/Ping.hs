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
import Effectful.DiscordChannel
import Effectful.DynamicLogger
import Effectful.NonDet
import RIO hiding ((^.))

-- | "ping"メッセージに応答して"pong"を返します。
pingEventHandler :: (DiscordChannel :> es, NonDet :> es, DynamicLogger :> es) => Response -> Eff es ()
pingEventHandler = \case
  InteractionCreate event -> do
    if (event ^. IC.slashCommandName) == "ping"
      then case makeContent "pong" of
        Success c -> do
          info "Dispatched Ping Handler"
          let params = makeSendMessageParams (event ^. IC.channelId) c Nothing False Nothing Nothing Nothing
          sendMessage params
          pure ()
        Failure _ -> pure ()
      else emptyEff
    pure ()
  _ -> emptyEff
