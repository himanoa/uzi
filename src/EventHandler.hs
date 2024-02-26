module EventHandler
  (
    dispatchEventHandlers
  )
where

import EventHandler.HelloEventHandler
import EventHandler.ReadyEventHandler
import Effectful.DynamicLogger (DynamicLogger)
import Effectful.DiscordGateway
import Effectful
import Data.Discord.Response
import Effectful.NonDet 
import Effectful.DiscordApiTokenReader (DiscordApiTokenReader)

dispatchEventHandlers :: (DiscordGateway :> es, DynamicLogger :> es, DiscordApiTokenReader :> es) =>  Response -> Eff es ()
dispatchEventHandlers  r = do
  _ <- runNonDet OnEmptyKeep (readyEventHandler r <|> helloEventHandler r)
  pure ()
--  dispatchMessageEventHandlers c r

