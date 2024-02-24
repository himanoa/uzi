{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module EventHandler.HelloEventHandler where

import Effectful.DiscordGateway (DiscordGateway, sendEvent)
import Effectful
import Data.Discord
import Data.Discord.Request.IdentifyRequest (defaultIdentifyRequest)
import EnvConfig
import Data.Text
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString.Char8 qualified as ByteString

convertToText :: String -> Text
convertToText = decodeUtf8 . ByteString.pack

helloEventHandler :: (DiscordGateway :> es) => EnvConfig -> Response -> Eff es ()
helloEventHandler config = \case
  Hello _ -> do
    sendEvent . Identify . defaultIdentifyRequest $ config.discordApiToken 
