{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module EventHandler.MessageCreateEventHandler.OrganizeTimes where

import Control.Lens
import Data.Discord
import Data.Discord.Content
import Data.Discord.Response.MessageCreateEventResponse qualified as MC
import Data.Text (pack)
import Data.Uzi.OrganizeTimes
import Effectful
import Effectful.DiscordChannel
import Effectful.DynamicLogger
import Effectful.Error.Dynamic
import Effectful.NonDet
import RIO hiding ((^.))

organizeTimesHandler :: (DiscordChannel :> es, NonDet :> es, DynamicLogger :> es) => Response -> Eff es ()
organizeTimesHandler = \case
  MessageCreate event -> do
    if body (event ^. MC.content) == Just "organize-times"
      then do
        info "organizeTimesHandler dispatched"
        sendMessage (makeMessage (event ^. MC.channelId) (makeUnsafeContent "times channelの整理を開始したよ！"))
        organizeTimesEither <- runError @OrganizeTimesError (organizeTimes (event ^. MC.guildId))
        case organizeTimesEither of
          Right _ -> do
            info "Organized times"
            sendMessage (makeMessage (event ^. MC.channelId) (makeUnsafeContent "times channelを整理したよ！"))
          Left (_, e) -> do
            attention "Failed organize times"
            sendMessage (makeMessage (event ^. MC.channelId) (makeUnsafeContent ("times channelの整理に失敗したよ！ " <> (pack . show $ e))))
      else emptyEff
  _ -> emptyEff
