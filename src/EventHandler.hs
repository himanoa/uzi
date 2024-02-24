module EventHandler
  (
    handlers
  )
where

import EventHandler.HelloEventHandler
import EventHandler.ReadyEventHandler
import Effectful.DynamicLogger (DynamicLogger)
import Effectful.DiscordGateway
import Effectful
import EnvConfig
import Data.Discord.Response

handlers :: (DiscordGateway :> es, DynamicLogger :> es) => EnvConfig -> Response -> Eff es ()
handlers = mconcat [
  readyEventHandler,
  helloEventHandler
  ]

