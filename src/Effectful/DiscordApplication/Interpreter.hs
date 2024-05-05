{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- |
-- Module: Effectful.DiscordApplication.Effect
-- Description: UziBotが使うDiscordのアクセストークンを取得するEffectのインタプリタです
-- Maintainer: himanoa <matsunoappy@gmail.com>
--
-- UziBotが使うDiscordのアクセストークンを取得するEffectのインタプリタです
module Effectful.DiscordApplication.Interpreter where

import Control.Exception
import Data.Aeson
import Data.Text
import Effectful
import Effectful.DiscordApiTokenReader
import Effectful.DiscordApplication.Effect
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Internal.Monad (unsafeEff_)
import Effectful.Req
import Network.HTTP.Req
import RIO

data FromEnvironmentError = DiscordApiTokenIsUndefined
  deriving (Show)

instance Exception FromEnvironmentError

-- | DiscordAPIのホスト部分を返す
host :: Text
host = "discord.com"

-- | DiscordAPIのversionを返す
version :: Text
version = "v10"

newtype Application = Application
  { __id :: ApplicationId
  }
  deriving (Show, Eq)

instance FromJSON Application where
  parseJSON = withObject "Application" $ \obj ->
    (obj .: "id") <&> (\parsed -> Application {__id = parsed})

applicationId :: Application -> ApplicationId
applicationId (Application {__id = app_id}) = app_id

-- | 'Data.Effectful.DiscordApplication' Effectを実行します
--
--  このインタプリタでは環境変数 UZI_DISCORD_API_TOKEN に入ったDiscordAPiTokenを取得します
runDiscordApplication :: (DiscordApiTokenReader :> es, Request :> es) => Eff (DiscordApplication : es) a -> Eff es a
runDiscordApplication = interpret $ \_ -> \case
  GetApplication -> do
    token <- getToken
    let pr = jsonResponse @Application

    response <-
      request GET (https host /: "api" /: version /: "applications/@me") NoReqBody pr
        $ header "Authorization" ("Bot " <> encodeUtf8 token)

    fmap applicationId (unsafeEff_ . getResponseBodyAsJsonResponse $ response)