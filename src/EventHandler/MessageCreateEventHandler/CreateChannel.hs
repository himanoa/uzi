{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module EventHandler.MessageCreateEventHandler.CreateChannel where

import Control.Lens
import Data.Discord
import Data.Discord.Content
import Data.Discord.Response.MessageCreateEventResponse qualified as MCE
import Data.Uzi.OrganizeTimes
import Effectful
import Effectful.DiscordChannel
import Effectful.DynamicLogger
import Effectful.Error.Dynamic
import Effectful.NonDet

createChannelEventHandler :: (DiscordChannel :> es, NonDet :> es, DynamicLogger :> es) => Response -> Eff es ()
createChannelEventHandler = \case
  MessageCreate res -> do
    -- FIXME: create-channel foobar みたいな構文のみ受けれるようにする
    let guildId = res ^. MCE.guildId
    info "CreateChannelEventHandler dispatched"
    createChannel guildId (makeCreateChannelParams . ChannelName $ "times-test")
    sendMessage (makeMessage (res ^. MCE.channelId) (makeUnsafeContent "timesを作ったよ -> #times-test"))
    _ <-
      (runError @OrganizeTimesError . organizeTimes $ guildId) >>= \case
        Right _ -> sendMessage (makeMessage (res ^. MCE.channelId) (makeUnsafeContent "times channelをソートしたよ"))
        Left _ -> pure ()
    pure ()
  _ -> emptyEff
