{-# LANGUAGE ImpredicativeTypes #-}
module EventHandler.MessageCreateEventHandler (
  dispatchMessageEventHandlers
) where

import EventHandler.MessageCreateEventHandler.Ping
import Effectful.DiscordGateway
import Effectful
import Effectful.DynamicLogger
import Data.Discord

dispatchMessageEventHandlers :: (DiscordGateway :> es, DynamicLogger :> es) => Response -> Eff es ()
dispatchMessageEventHandlers =
  pingEventHandler
