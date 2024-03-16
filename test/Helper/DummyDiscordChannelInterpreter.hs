{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module Helper.DummyDiscordChannelInterpreter (
  runDummyDiscordChannel
) where

import Effectful
import Effectful.DiscordChannel
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.State.Static.Local
import Effectful.DiscordChannel.Effect

runDummyDiscordChannel :: (State (Maybe SendMessageParams) :> es) => Eff (DiscordChannel : es) a -> Eff es a
runDummyDiscordChannel = interpret $ \_ -> \case
  SendMessage params -> modify .const . Just $ params
