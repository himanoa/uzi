{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- |
-- Module: Effectful.DiscordApplication.Effect
-- Description: UziBotのApplication Idを取得するEffect
-- Maintainer: himanoa <matsunoappy@gmail.com>
--
-- UziBotのApplication Idを読み出すためのEffect Moduleです
module Effectful.DiscordApplication.Effect
  ( DiscordApplication (..),
    getApplication,
    ApplicationId (..),
  )
where

import Data.Aeson
import Data.Eq
import Data.Text qualified as DT
import Effectful
import Effectful.Dispatch.Dynamic (send)
import GHC.Show

-- | UziBotのApplication Idを読み出すためのEffectの定義
data DiscordApplication :: Effect where
  GetApplication :: DiscordApplication m ApplicationId

type instance DispatchOf DiscordApplication = Dynamic

newtype ApplicationId = ApplicationId DT.Text
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via DT.Text

-- | UziBotのApplicationを取得します。
getApplication :: (DiscordApplication :> es) => Eff es ApplicationId
getApplication = send GetApplication
