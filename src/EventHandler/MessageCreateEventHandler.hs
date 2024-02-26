{-# LANGUAGE ImpredicativeTypes #-}

module EventHandler.MessageCreateEventHandler
  ( dispatchMessageEventHandlers,
  )
where

import Data.Discord
import Effectful
import Effectful.DiscordGateway
import Effectful.DynamicLogger
import EventHandler.MessageCreateEventHandler.Ping

dispatchMessageEventHandlers :: (DiscordGateway :> es, DynamicLogger :> es) => Response -> Eff es ()
dispatchMessageEventHandlers =
  pingEventHandler
