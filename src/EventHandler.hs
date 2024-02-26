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
import EnvConfig
import Data.Discord.Response
import Effectful.NonDet 

dispatchEventHandlers :: (DiscordGateway :> es, DynamicLogger :> es) => EnvConfig -> Response -> Eff es ()
dispatchEventHandlers c r = do
  _ <- runNonDet OnEmptyKeep (readyEventHandler c r <|> helloEventHandler c r)
  pure ()
--  dispatchMessageEventHandlers c r

