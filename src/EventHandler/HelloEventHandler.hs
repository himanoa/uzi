{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module EventHandler.HelloEventHandler where

import Data.ByteString.Char8 qualified as ByteString
import Data.Discord
import Data.Discord.Request.IdentifyRequest (defaultIdentifyRequest)
import Data.Text
import Data.Text.Encoding (decodeUtf8)
import Effectful
import Effectful.DiscordGateway (DiscordGateway, sendEvent)
import Effectful.DynamicLogger
import Effectful.NonDet
import Effectful.DiscordApiTokenReader (DiscordApiTokenReader, getToken)

convertToText :: String -> Text
convertToText = decodeUtf8 . ByteString.pack

helloEventHandler :: (DiscordGateway :> es, DynamicLogger :> es, NonDet :> es, DiscordApiTokenReader :> es) => Response -> Eff es ()
helloEventHandler = \case
  Hello _ -> do
    discordApiToken <- getToken
    sendEvent . Identify . defaultIdentifyRequest $ discordApiToken
    info "Sent HelloEvetnt"
  _ -> emptyEff
