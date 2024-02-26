{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module EventHandler.ReadyEventHandler where

import Data.Discord.Response
import Data.String.Conversions (ConvertibleStrings (convertString))
import Effectful
import Effectful.DynamicLogger
import Effectful.NonDet

readyEventHandler :: (DynamicLogger :> es, NonDet :> es) => Response -> Eff es ()
readyEventHandler = \case
  Ready r -> do
    info "Dispatch ReadyEventHandler"
    info . convertString . show $ r
  _ -> emptyEff
