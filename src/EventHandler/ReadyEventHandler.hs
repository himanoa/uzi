{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module EventHandler.ReadyEventHandler where

import Data.Discord.Response
import Data.String.Conversions (ConvertibleStrings (convertString))
import Effectful
import Effectful.DynamicLogger
import Effectful.NonDet
import Control.Lens
import Data.Discord.Response.ReadyEventResponse
import Effectful.BotUser.Effect

readyEventHandler :: (DynamicLogger :> es, NonDet :> es, BotUser :> es) => Response -> Eff es ()
readyEventHandler = \case
  Ready r -> do
    info "Dispatch ReadyEventHandler"
    info . convertString . show $ r
    setBotUser (r ^. user)
  _ -> emptyEff
