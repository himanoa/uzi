{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module EventHandler.ReadyEventHandler where

import Control.Lens
import Data.Discord.Response
import Data.Discord.Response.ReadyEventResponse
import Effectful
import Effectful.BotUser.Effect
import Effectful.DynamicLogger
import Effectful.NonDet
import RIO (displayShow)

readyEventHandler :: (DynamicLogger :> es, NonDet :> es, BotUser :> es) => Response -> Eff es ()
readyEventHandler = \case
  Ready r -> do
    info "Dispatch ReadyEventHandler"
    info . displayShow $ r
    setBotUser (r ^. user)
  _ -> emptyEff
