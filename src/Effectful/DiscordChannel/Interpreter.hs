{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Effectful.DiscordChannel.Interpreter where

import Effectful.DiscordApiTokenReader (DiscordApiTokenReader, getToken)
import Effectful
import Effectful.Req (Request, request)
import Effectful.DiscordChannel.Effect
import Effectful.Dispatch.Dynamic (interpret)
import Network.HTTP.Req (POST(POST), https, ReqBodyJson (ReqBodyJson), ignoreResponse, header, (/:))
import Data.Text (Text)
import Data.Aeson
import Data.Text.Encoding
import Data.Discord.ChannelId
import Control.Lens
import Effectful.DynamicLogger
import Data.Coerce (coerce)
import Data.String.Conversions

host :: Text
host = "discord.com"

runDiscordChannel :: (DiscordApiTokenReader :> es, Request :> es, DynamicLogger :> es) => Eff (DiscordChannel : es) a -> Eff es a
runDiscordChannel = interpret $ \_ ->  \case
  SendMessage params  -> do
    token <- getToken
    info . coerce $ params ^. channelId
    info . convertString . encode $ params
    _ <- request POST (https host /: "api" /: "v10" /: "channels" /: (coerce $ params ^. channelId) /: "messages" ) (ReqBodyJson . toJSON $ params) ignoreResponse $
      header "Authorization" ("Bot " <> encodeUtf8 token)
    pure ()
