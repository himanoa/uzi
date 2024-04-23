{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module EventHandler.MessageCreateEventHandler.Help where

import RIO hiding ((^.))
import Control.Lens
import Data.Discord
import Data.Discord.Content (body)
import Data.Discord.Response.MessageCreateEventResponse qualified as MC
import Data.Either.Validation
import Effectful
import Effectful.DiscordChannel
import Effectful.DynamicLogger
import Effectful.NonDet

helpEventHandler :: (DiscordChannel :> es, NonDet :> es, DynamicLogger :> es) => Response -> Eff es ()
helpEventHandler = \case
  MessageCreate event -> do
    if body (event ^. MC.content) == Just "help"
      then case makeContent "https://github.com/himanoa/uzi/blob/master/docs/HELP.md" of
        Success c -> do
          info "Dispatched Help Handler"
          let params = makeSendMessageParams (event ^. MC.channelId) c Nothing False Nothing Nothing Nothing
          sendMessage params
          pure ()
        Failure _ -> pure ()
      else emptyEff
    pure ()
  _ -> emptyEff
