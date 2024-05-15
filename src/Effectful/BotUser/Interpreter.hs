{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- |
-- Module: Effectful.BotUser.Interpreter
-- Description: UziBotがログインしているDiscordアカウントの情報へアクセスするEffectのインタプリタ
-- Maintainer: himanoa <matsunoappy@gmail.com>
--
-- UziBotがログインしているDiscordアカウントの情報へアクセスするEffectのインタプリタ
module Effectful.BotUser.Interpreter where

import Data.Discord.User (User)
import Effectful
import Effectful.BotUser.Effect
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.State.Static.Shared
import RIO

-- | BotUserEffectを実行します
runBotUser :: (State (Maybe User) :> es) => Eff (BotUser : es) a -> Eff es a
runBotUser = interpret $ \_ -> \case
  SetBotUser u -> do
    _ <- put $ Just u
    pure ()
  GetBotUser -> get
