{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- |
-- Module: EventHandler.HelloEventHandler
-- Description: Discordが送信してくるHELLOイベントのイベントハンドラです
-- Maintainer: himanoa <matsunoappy@gmail.com>
--
-- Discordが送信してくるHELLOイベントのイベントハンドラです。
--
-- 詳細: https://discord.com/developers/docs/topics/gateway-events#hello
module EventHandler.HelloEventHandler where

import Control.Lens
import Data.Discord
import Data.Discord.Response.HelloEventResponse (interval)
import Data.Uzi.HeartbeatInterval (HeartbeatInterval, makeHeartbeatInterval)
import Effectful
import Effectful.DiscordApiTokenReader (DiscordApiTokenReader, getToken)
import Effectful.DiscordGateway (DiscordGateway, sendEvent)
import Effectful.DynamicLogger
import Effectful.NonDet
import Effectful.State.Static.Shared
import RIO hiding ((^.))

helloEventHandler :: (DiscordGateway :> es, DynamicLogger :> es, NonDet :> es, DiscordApiTokenReader :> es, State (Maybe HeartbeatInterval) :> es) => Response -> Eff es ()
helloEventHandler = \case
  Hello e -> do
    let interval' = e ^. interval
    discordApiToken <- getToken
    sendEvent . Identify . defaultIdentifyRequest $ discordApiToken
    info "Sent HelloEvetnt"
    info "Fork heartbeart send task"
    RIO.void $ put . Just . makeHeartbeatInterval $ interval'
    info ("Put heartbeat interval " <> RIO.displayShow interval')
  _ -> emptyEff
