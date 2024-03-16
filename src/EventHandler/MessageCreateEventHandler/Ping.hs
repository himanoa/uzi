{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module EventHandler.MessageCreateEventHandler.Ping where

import Data.Discord
import Effectful
import Effectful.DiscordChannel
import Data.Either.Validation
import Data.Discord.Response.MessageCreateEventResponse qualified as MC
import Control.Lens
import Effectful.NonDet
import Data.Discord.Content (makeUnsafeContent)

pingEventHandler :: ( DiscordChannel :> es, NonDet :> es) =>Response -> Eff es ()
pingEventHandler = \case
  MessageCreate event -> do
    if (event ^. MC.content) == makeUnsafeContent "ping"
      then case makeContent "pong" of
        Success c -> do
          let params = makeSendMessageParams (event ^. MC.channelId) c Nothing False Nothing Nothing Nothing
          sendMessage params
          pure ()
        Failure _ -> pure ()
      else emptyEff
    pure ()
  _ -> emptyEff
