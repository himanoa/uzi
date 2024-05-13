{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- |
-- Module: EventHandler.MessageCreateEventHandler.Interaction
-- Description: Discordのinteractionのcallbackを行います。
-- Maintainer: himanoa <matsunoappy@gmail.com>
module EventHandler.MessageCreateEventHandler.InteractionCallback where

import Data.Discord
import Effectful
import Effectful.InteractionCallback
import Effectful.NonDet

interactionCallback :: ( NonDet :> es, InteractionCallback :> es) => Response -> Eff es ()
interactionCallback = \case
  InteractionCreate event -> do
    doLoadingCallback event
  _ -> emptyEff
