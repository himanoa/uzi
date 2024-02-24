{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Effectful.DiscordGateway.Effect where

import Effectful
import Data.Text
import Effectful.Dispatch.Dynamic (HasCallStack, send)

data DiscordGateway :: Effect where
  ReceiveEvent :: DiscordGateway m Text
  SendEvent :: Text -> DiscordGateway m ()

type instance DispatchOf DiscordGateway = Dynamic

receiveEvent :: (HasCallStack, DiscordGateway :> es) => Eff es Text
receiveEvent = send ReceiveEvent

sendEvent :: (HasCallStack, DiscordGateway :> es) => Text -> Eff es ()
sendEvent payload = send (SendEvent payload)
