{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Helper.DummyDiscordChannelInterpreter
  ( runDummyDiscordChannel,
  )
where

import Effectful
import Effectful.DiscordChannel
import Effectful.DiscordChannel.Effect
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.State.Static.Local

runDummyDiscordChannel :: (State (Maybe SendMessageParams) :> es) => Eff (DiscordChannel : es) a -> Eff es a
runDummyDiscordChannel = interpret $ \_ -> \case
  SendMessage params -> modify . const . Just $ params
  CreateChannel _ _ -> pure ()
