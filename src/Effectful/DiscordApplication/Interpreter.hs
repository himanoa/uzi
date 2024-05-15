{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- |
-- Module: Effectful.DiscordApplication.Effect
-- Description: Application Idを取得するEffectのインタプリタです
-- Maintainer: himanoa <matsunoappy@gmail.com>
--
-- UziBot自身のApplication Idを取得するEffectのインタプリタです
module Effectful.DiscordApplication.Interpreter
  ( applicationId,
    runDiscordApplication,
  )
where

import Control.Exception
import Data.Aeson
import Effectful
import Effectful.DiscordApiTokenReader
import Effectful.DiscordApplication.Effect
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Internal.Monad (unsafeEff_)
import Effectful.Req
import Network.HTTP.Req
import RIO
import RIO.Text qualified as T

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
  { _id :: ApplicationId
  }
  deriving (Show, Eq)

instance FromJSON Application where
  parseJSON = withObject "Application" $ \obj ->
    (obj .: "id") <&> (\parsed -> Application {_id = parsed})

applicationId :: Application -> ApplicationId
applicationId (Application {_id = app_id}) = app_id

-- | 'Data.Effectful.DiscordApplication' Effectを実行します
--
--  このインタプリタではAPIにアクセスして、自身のApplication Idを取得します
runDiscordApplication :: (DiscordApiTokenReader :> es, Request :> es) => Eff (DiscordApplication : es) a -> Eff es a
runDiscordApplication = interpret $ \_ -> \case
  GetApplication -> do
    token <- getToken
    let pr = jsonResponse @Application

    response <-
      request GET (https host /: "api" /: version /: "applications/@me") NoReqBody pr
        $ header "Authorization" ("Bot " <> T.encodeUtf8 token)

    fmap applicationId (unsafeEff_ . getResponseBodyAsJsonResponse $ response)
