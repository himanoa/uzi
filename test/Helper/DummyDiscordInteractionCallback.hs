{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Helper.DummyDiscordInteractionCallback
  ( runDummyInteractionCallback,
  )
where

import Data.Discord
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.InteractionCallback (InteractionCallback (..))
import Effectful.State.Static.Local

runDummyInteractionCallback :: (State (Maybe Content) :> es) => Eff (InteractionCallback : es) a -> Eff es a
runDummyInteractionCallback = interpret $ \_ -> \case
  ChannelMessage _ param _ -> modify . const . Just $ param
  Loading _ -> do
    pure ()
