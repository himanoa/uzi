{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Effectful.BotUser.Interpreter where
import Data.Discord.User (User)
import Effectful.State.Static.Shared
import Effectful
import Effectful.BotUser.Effect
import Effectful.Dispatch.Dynamic (interpret)

runBotUser :: (State (Maybe User) :> es)  => Eff (BotUser : es) a -> Eff es a
runBotUser = interpret $ \_ -> \case
  SetBotUser u -> do
    _ <- put $ Just u
    pure ()
  GetBotUser -> get
