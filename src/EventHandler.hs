module EventHandler
  ( dispatchEventHandlers,
  )
where

import Data.Discord.Response
import Effectful
import Effectful.DiscordApiTokenReader (DiscordApiTokenReader)
import Effectful.DiscordGateway
import Effectful.DynamicLogger (DynamicLogger)
import Effectful.NonDet
import EventHandler.HelloEventHandler
import EventHandler.ReadyEventHandler

dispatchEventHandlers :: (DiscordGateway :> es, DynamicLogger :> es, DiscordApiTokenReader :> es) => Response -> Eff es ()
dispatchEventHandlers r = do
  _ <- runNonDet OnEmptyKeep (readyEventHandler r <|> helloEventHandler r)
  pure ()

--  dispatchMessageEventHandlers c r
