{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- |
-- Module: Effectful.DiscordSlash.Interpreter
-- Description: 'Effectful.DiscordSlash.Effect' を実行するインタプリタです。
-- Maintainer: himanoa <matsunoappy@gmail.com>
--
-- 'Effectful.DiscordSlash.Effect' を実行するインタプリタです。
module Effectful.DiscordSlash.Interpreter
  ( runRegisterSlash,
  )
where

import Data.Aeson
import Effectful
import Effectful.DiscordApiTokenReader (DiscordApiTokenReader, getToken)
import Effectful.DiscordApplication.Effect
  ( ApplicationId (..),
    DiscordApplication,
    getApplication,
  )
import Effectful.DiscordSlash.Effect (SlashCommand (..))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.DynamicLogger.Effect
import Effectful.Req (Request, request)
import Network.HTTP.Req
import RIO hiding ((^.))

-- | DiscordAPIのホスト部分を返す
host :: Text
host = "discord.com"

-- | DiscordAPIのversionを返す
version :: Text
version = "v10"

-- | スラッシュコマンド登録のAPIを呼び出します。
runRegisterSlash :: (DiscordApiTokenReader :> es, Request :> es, DiscordApplication :> es, DynamicLogger :> es) => Eff (SlashCommand : es) a -> Eff es a
runRegisterSlash = interpret $ \_ -> \case
  GlobalCommand name desc params -> do
    token <- getToken
    ApplicationId app <- getApplication

    let body = object ["name" .= name, "type" .= (1 :: Integer), "description" .= desc, "options" .= params]

    _ <- info "POST Register Slash"
    _ <-
      request POST (https host /: "api" /: version /: "applications" /: app /: "commands") (ReqBodyJson . toJSON $ body) ignoreResponse
        $ header "Authorization" ("Bot " <> encodeUtf8 token)
    pure ()
