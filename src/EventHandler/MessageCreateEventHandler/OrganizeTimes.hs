{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module EventHandler.MessageCreateEventHandler.OrganizeTimes where

import Effectful.DiscordChannel
import Effectful
import Effectful.NonDet
import Effectful.DynamicLogger
import Data.Discord
import Data.Discord.Content
import Data.Discord.Response.MessageCreateEventResponse qualified as MC
import Control.Lens

organizeTimesHandler :: (DiscordChannel :> es, NonDet :> es, DynamicLogger :> es) => Response -> Eff es ()

organizeTimesHandler = \case
  MessageCreate event -> do
    info "organizeTimesHandler dispatched"
    if body (event ^. MC.content) == Just "organize-times" 
      then do
        sendMessage (makeMessage (event ^. MC.channelId) (makeUnsafeContent "times channelの整理を開始したよ！"))
      else emptyEff
  _ -> emptyEff

