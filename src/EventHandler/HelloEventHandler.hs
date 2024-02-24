{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module EventHandler.HelloEventHandler where

import Effectful.DiscordGateway (DiscordGateway, sendEvent)
import Effectful
import Data.Discord
import Data.Discord.Request.IdentifyRequest (defaultIdentifyRequest)
import EnvConfig

helloEventHandler :: (DiscordGateway :> es) => EnvConfig -> Response -> Eff es ()
helloEventHandler config res = do
  case res of
    Hello _ -> do
      sendEvent . Identify . defaultIdentifyRequest $ config.discordApiToken 
