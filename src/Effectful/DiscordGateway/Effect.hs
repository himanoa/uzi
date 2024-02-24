{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Effectful.DiscordGateway.Effect where

import Data.Discord.Response (Response)
import Data.Text
import Effectful
import Effectful.Dispatch.Dynamic (HasCallStack, send)

data DiscordGateway :: Effect where
  ReceiveEvent :: DiscordGateway m (Maybe Response)
  SendEvent :: Text -> DiscordGateway m ()

type instance DispatchOf DiscordGateway = Dynamic

receiveEvent :: (HasCallStack, DiscordGateway :> es) => Eff es (Maybe Response)
receiveEvent = send ReceiveEvent

sendEvent :: (HasCallStack, DiscordGateway :> es) => Text -> Eff es ()
sendEvent payload = send (SendEvent payload)
