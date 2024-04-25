{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

{-|
 Module: Effectful.DiscordApiTokenReader.Effect
 Description: UziBotが使うDiscordのアクセストークンを取得するEffectのインタプリタです
 Maintainer: himanoa <matsunoappy@gmail.com>

 UziBotが使うDiscordのアクセストークンを取得するEffectのインタプリタです
-}
module Effectful.DiscordApiTokenReader.Interpreter where

import Control.Exception
import Data.String.Conversions (ConvertibleStrings (convertString))
import Effectful
import Effectful.DiscordApiTokenReader.Effect
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Environment
import RIO

data FromEnvironmentError = DiscordApiTokenIsUndefined
  deriving (Show)

instance Exception FromEnvironmentError

-- | 'Data.Effectful.DiscordApiTokenReader' Effectを実行します
--
--  このインタプリタでは環境変数 UZI_DISCORD_API_TOKEN に入ったDiscordAPiTokenを取得します
runDiscordApiTokenReader :: (Environment :> es) => Eff (DiscordApiTokenReader : es) a -> Eff es a
runDiscordApiTokenReader = interpret $ \_ -> \case
  GetToken ->
    lookupEnv "UZI_DISCORD_API_TOKEN" >>= \case
      Just token -> pure . convertString $ token
      Nothing -> throw DiscordApiTokenIsUndefined
