{-# LANGUAGE ImpredicativeTypes #-}
module EventHandler.MessageCreateEventHandler (
  dispatchMessageEventHandlers
) where

import EventHandler.MessageCreateEventHandler.Ping
import Effectful.DiscordGateway
import Effectful
import Effectful.DynamicLogger
import EnvConfig
import Data.Discord

dispatchMessageEventHandlers :: (DiscordGateway :> es, DynamicLogger :> es) => EnvConfig -> Response -> Eff es ()
dispatchMessageEventHandlers =
  pingEventHandler
