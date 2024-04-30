{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- |
-- Module: Effectful.BotUser.Effect
-- Description: UziBotがログインしているDiscordアカウントの情報へアクセスするEffect
-- Maintainer: himanoa <matsunoappy@gmail.com>
--
-- UziBotがログインしているDiscordアカウントの情報へアクセスするEffectです。
--
-- Botがログインしているユーザーが確定するのはUziを起動してDiscordからReadyEventを受け取った後です。
module Effectful.BotUser.Effect where

import Data.Discord.User
import Effectful
import Effectful.Dispatch.Dynamic
import RIO

data BotUser :: Effect where
  SetBotUser :: User -> BotUser m ()
  GetBotUser :: BotUser m (Maybe User)

type instance DispatchOf BotUser = Dynamic

-- | Botがログイン中のDiscordアカウントを更新します
--
-- 'EventHandler.ReadyEventHandler.readyEventHandler'以外からは呼び出さないでください
setBotUser :: (HasCallStack, BotUser :> es) => User -> Eff es ()
setBotUser = send . SetBotUser

-- | Botがログイン中のDiscordアカウントの情報を取得します
--
-- ログイン中のDiscordアカウントが確定しない場合は'Nothing'を返します
getBotUser :: (HasCallStack, BotUser :> es) => Eff es (Maybe User)
getBotUser = send GetBotUser
