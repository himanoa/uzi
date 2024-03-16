{-# LANGUAGE ImpredicativeTypes #-}

module EventHandler.MessageCreateEventHandler
  ( dispatchMessageEventHandlers,
  )
where

import Data.Discord
import Effectful
import Effectful.DynamicLogger
import Effectful.DiscordChannel
import EventHandler.MessageCreateEventHandler.Ping
import Effectful.NonDet
import Control.Lens
import Data.Discord.Response.MessageCreateEventResponse (isBot)

dispatchMessageEventHandlers :: (DynamicLogger :> es, DiscordChannel :> es, NonDet :> es) => Response -> Eff es ()
dispatchMessageEventHandlers res = case res of
  MessageCreate e -> 
    if e ^. isBot
      then emptyEff
      else pingEventHandler res
  _ -> emptyEff
