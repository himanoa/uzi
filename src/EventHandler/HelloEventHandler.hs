{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
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
import EnvConfig

convertToText :: String -> Text
convertToText = decodeUtf8 . ByteString.pack

helloEventHandler :: (DiscordGateway :> es, DynamicLogger :> es) => EnvConfig -> Response -> Eff es ()
helloEventHandler config = \case
  Hello _ -> do
    sendEvent . Identify . defaultIdentifyRequest $ config.discordApiToken
    info "Sent HelloEvetnt"
  _ -> pure()
