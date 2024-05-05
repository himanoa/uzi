{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE DerivingVia #-}

-- |
-- Module: Effectful.DiscordApplication.Effect
-- Description: UziBotが使うDiscordのアクセストークンを取得するEffect
-- Maintainer: himanoa <matsunoappy@gmail.com>
--
-- UziBotが使うDiscordのアクセストークンを読み出すためのEffect Moduleです
module Effectful.DiscordApplication.Effect where

import Effectful
import Effectful.Dispatch.Dynamic (send)
import Data.Aeson
import Data.Eq
import GHC.Show
import qualified Data.Text as DT

-- | UziBotが使うDiscordのアクセストークンを読み出すためのEffectの定義
data DiscordApplication :: Effect where
  GetApplication :: DiscordApplication m ApplicationId

type instance DispatchOf DiscordApplication = Dynamic

newtype ApplicationId = ApplicationId DT.Text
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via DT.Text


-- | DiscordAPIにアクセスするためのToken文字列を取得します。
--
-- このAPIはEventHandlerなどからは直接読み出さないでください。
--
-- DiscordのAPIを使いたい場合は 'Effectful.DiscordChannel' や 'Effectful.DiscordGateway' などのこれらをラップしたEffectを使用してください。
getApplication :: (DiscordApplication :> es) => Eff es ApplicationId
getApplication = send GetApplication
