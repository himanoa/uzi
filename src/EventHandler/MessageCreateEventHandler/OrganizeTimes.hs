{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module EventHandler.MessageCreateEventHandler.OrganizeTimes where

import Control.Lens
import Data.Discord
import Data.Discord.Content
import Data.Discord.Response.MessageCreateEventResponse qualified as MC
import Effectful
import Effectful.DiscordChannel
import Effectful.DiscordChannel.Effect (getChannels)
import Effectful.DynamicLogger
import Effectful.NonDet
import RIO qualified

organizeTimesHandler :: (DiscordChannel :> es, NonDet :> es, DynamicLogger :> es) => Response -> Eff es ()
organizeTimesHandler = \case
  MessageCreate event -> do
    info "organizeTimesHandler dispatched"
    if body (event ^. MC.content) == Just "organize-times"
      then do
        channels <- getChannels (event ^. MC.guildId)
        info . RIO.displayShow $ channels
        sendMessage (makeMessage (event ^. MC.channelId) (makeUnsafeContent "times channelの整理を開始したよ！"))
      else emptyEff
  _ -> emptyEff
