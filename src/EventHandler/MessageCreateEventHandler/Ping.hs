{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module EventHandler.MessageCreateEventHandler.Ping where

import Control.Lens
import Data.Discord
import Data.Discord.Content (body)
import Data.Discord.Response.MessageCreateEventResponse qualified as MC
import Data.Either.Validation
import Effectful
import Effectful.DiscordChannel
import Effectful.NonDet

pingEventHandler :: (DiscordChannel :> es, NonDet :> es) => Response -> Eff es ()
pingEventHandler = \case
  MessageCreate event -> do
    if body (event ^. MC.content) == Just "ping"
      then case makeContent "pong" of
        Success c -> do
          let params = makeSendMessageParams (event ^. MC.channelId) c Nothing False Nothing Nothing Nothing
          sendMessage params
          pure ()
        Failure _ -> pure ()
      else emptyEff
    pure ()
  _ -> emptyEff
