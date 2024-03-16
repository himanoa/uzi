{-# LANGUAGE ImpredicativeTypes #-}

module EventHandler.MessageCreateEventHandler
  ( dispatchMessageEventHandlers,
  )
where

import Data.Discord
import Effectful
import Effectful.DynamicLogger
import EventHandler.MessageCreateEventHandler.Ping

dispatchMessageEventHandlers :: ( DynamicLogger :> es) =>Response -> Eff es ()
dispatchMessageEventHandlers =
  pingEventHandler
