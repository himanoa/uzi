module EventHandler
  ( dispatchEventHandlers,
  )
where

import RIO
import Data.Discord.Response
import Data.Uzi.HeartbeatInterval
import Effectful
import Effectful.BotUser
import Effectful.DiscordApiTokenReader (DiscordApiTokenReader)
import Effectful.DiscordChannel
import Effectful.DiscordGateway
import Effectful.DynamicLogger (DynamicLogger)
import Effectful.NonDet
import Effectful.State.Static.Shared
import EventHandler.HelloEventHandler
import EventHandler.MessageCreateEventHandler
import EventHandler.ReadyEventHandler

dispatchEventHandlers :: (DiscordGateway :> es, DynamicLogger :> es, DiscordApiTokenReader :> es, DiscordChannel :> es, BotUser :> es, State (Maybe HeartbeatInterval) :> es) => Response -> Eff es ()
dispatchEventHandlers r = do
  _ <- runNonDet OnEmptyKeep (readyEventHandler r <|> helloEventHandler r <|> dispatchMessageEventHandlers r)
  pure ()
