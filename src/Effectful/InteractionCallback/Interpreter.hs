{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- |
-- Module: Effectful.InteractionCallback.Effect
-- Description: Application Idを取得するEffectのインタプリタです
-- Maintainer: himanoa <matsunoappy@gmail.com>
--
-- UziBot自身のApplication Idを取得するEffectのインタプリタです
module Effectful.InteractionCallback.Interpreter
  (
    runInteractionCallback,
  )
where

import Control.Exception
import Data.Aeson
import Data.Text
import Effectful
import Effectful.DiscordApiTokenReader
import Effectful.InteractionCallback.Effect
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Req
import Network.HTTP.Req
import RIO
import Effectful.DynamicLogger.Effect
import Data.Discord.Response.InteractionCreateEventResponse qualified as IC

data FromEnvironmentError = DiscordApiTokenIsUndefined
  deriving (Show)

instance Exception FromEnvironmentError

-- | DiscordAPIのホスト部分を返す
host :: Text
host = "discord.com"

-- | DiscordAPIのversionを返す
version :: Text
version = "v10"

-- | 'Data.Effectful.InteractionCallback' Effectを実行します
--
--  このインタプリタではAPIにアクセスして、自身のApplication Idを取得します
runInteractionCallback :: (DiscordApiTokenReader :> es, Request :> es, DynamicLogger :> es) => Eff (InteractionCallback : es) a -> Eff es a
runInteractionCallback = interpret $ \_ -> \case

  ChannelMessage event text flags -> do
    let IC.InteractionId _interactionId = event ^. IC.interactionId
    let IC.InteractionToken _interactionToken = event ^. IC.token
    token <- getToken
    let body = object [ "type" .= (4 :: Integer), "data" .= object ["content" .= text, "flags" .= flags]]

    _ <- info "POST Callback"
    _ <-
      request POST (https host /: "api" /: version /: "interactions" /: _interactionId /: _interactionToken /: "callback") (ReqBodyJson . toJSON $ body) ignoreResponse
        $ header "Authorization" ("Bot " <> encodeUtf8 token)
    pure ()
  Loading event -> do
    let IC.InteractionId _interactionId = event ^. IC.interactionId
    let IC.InteractionToken _interactionToken = event ^. IC.token
    token <- getToken

    let body = object [ "type" .= (5 :: Integer), "data" .= object []]

    _ <- info "POST Callback"
    _ <-
      request POST (https host /: "api" /: version /: "interactions" /: _interactionId /: _interactionToken /: "callback") (ReqBodyJson . toJSON $ body) ignoreResponse
        $ header "Authorization" ("Bot " <> encodeUtf8 token)
    pure ()

