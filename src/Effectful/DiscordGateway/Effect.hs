{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Effectful.DiscordGateway.Effect where

import Data.Discord.Request
import Data.Discord.Response
import Effectful
import Effectful.Dispatch.Dynamic (HasCallStack, send)

data DiscordGateway :: Effect where
  ReceiveEvent :: DiscordGateway m (Maybe Response)
  SendEvent :: Request -> DiscordGateway m ()

type instance DispatchOf DiscordGateway = Dynamic

receiveEvent :: (HasCallStack, DiscordGateway :> es) => Eff es (Maybe Response)
receiveEvent = send ReceiveEvent

sendEvent :: (HasCallStack, DiscordGateway :> es) => Request -> Eff es ()
sendEvent payload = send (SendEvent payload)
