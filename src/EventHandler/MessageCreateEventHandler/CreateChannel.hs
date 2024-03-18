{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module EventHandler.MessageCreateEventHandler.CreateChannel where

import Control.Lens
import Data.Discord
import Data.Discord.Response.MessageCreateEventResponse qualified as MCE
import Effectful
import Effectful.DiscordChannel
import Effectful.DynamicLogger
import Effectful.NonDet

createChannelEventHandler :: (DiscordChannel :> es, NonDet :> es, DynamicLogger :> es) => Response -> Eff es ()
createChannelEventHandler = \case
  MessageCreate res -> do
    -- FIXME: create-channel foobar みたいな構文のみ受けれるようにする
    let guildId = res ^. MCE.guildId
    info "CreateChannelEventHandler dispatched"
    createChannel guildId (makeCreateChannelParams . ChannelName $ "test")
    pure ()
  _ -> emptyEff
