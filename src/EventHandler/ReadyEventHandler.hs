{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module EventHandler.ReadyEventHandler where

import Control.Lens
import Data.Discord.Response
import Data.Discord.Response.ReadyEventResponse
import Data.String.Conversions (ConvertibleStrings (convertString))
import Effectful
import Effectful.BotUser.Effect
import Effectful.DynamicLogger
import Effectful.NonDet

readyEventHandler :: (DynamicLogger :> es, NonDet :> es, BotUser :> es) => Response -> Eff es ()
readyEventHandler = \case
  Ready r -> do
    info "Dispatch ReadyEventHandler"
    info . convertString . show $ r
    setBotUser (r ^. user)
  _ -> emptyEff
