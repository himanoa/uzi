{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Effectful.DiscordGateway.Effect where

import Effectful
import Data.Text
import Effectful.Dispatch.Dynamic (HasCallStack, send)
import Data.Discord.Response (Response)

data DiscordGateway :: Effect where
  ReceiveEvent :: DiscordGateway m Response
  SendEvent :: Text -> DiscordGateway m ()

type instance DispatchOf DiscordGateway = Dynamic

receiveEvent :: (HasCallStack, DiscordGateway :> es) => Eff es Response
receiveEvent = send ReceiveEvent

sendEvent :: (HasCallStack, DiscordGateway :> es) => Text -> Eff es ()
sendEvent payload = send (SendEvent payload)
