{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Helper.DummyDiscordGatewayInterpreter
  ( runDummyDiscordGateway,
  )
where

import Data.Discord
import Effectful
import Effectful.DiscordGateway.Effect
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.State.Static.Local

runDummyDiscordGateway :: (State (Maybe Request) :> es) => Eff (DiscordGateway : es) a -> Eff es a
runDummyDiscordGateway = interpret $ \_ -> \case
  ReceiveEvent -> undefined
  SendEvent request -> do
    modify . const . Just $ request
