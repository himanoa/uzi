{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE DerivingVia #-}

-- |
-- Module: Effectful.DiscordApplication.Effect
-- Description: UziBotのApplication Idを取得するEffect
-- Maintainer: himanoa <matsunoappy@gmail.com>
--
-- UziBotのApplication Idを読み出すためのEffect Moduleです
module Effectful.DiscordApplication.Effect where

import Effectful
import Effectful.Dispatch.Dynamic (send)
import Data.Aeson
import Data.Eq
import GHC.Show
import qualified Data.Text as DT

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
