{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module EventHandler.HelloEventHandler where

import Control.Lens
import Data.ByteString.Char8 qualified as ByteString
import Data.Discord
import Data.Discord.Response.HelloEventResponse (interval)
import Data.Text
import Data.Text.Encoding (decodeUtf8)
import Data.Uzi.HeartbeatInterval (HeartbeatInterval, makeHeartbeatInterval)
import Effectful
import Effectful.DiscordApiTokenReader (DiscordApiTokenReader, getToken)
import Effectful.DiscordGateway (DiscordGateway, sendEvent)
import Effectful.DynamicLogger
import Effectful.NonDet
import Effectful.State.Static.Shared
import RIO qualified

convertToText :: String -> Text
convertToText = decodeUtf8 . ByteString.pack

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

-- sendHeartbeat  :: (DiscordGateway :> es, Concurrent :> es) => Int -> Eff es ()
-- sendHeartbeat interval = do
--   sendEvent Heartbeat
--   void . threadDelay $ (interval * (1 :: Int))
