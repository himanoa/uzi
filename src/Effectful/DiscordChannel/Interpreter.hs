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
import Network.HTTP.Req (POST(POST), https, ReqBodyJson (ReqBodyJson), ignoreResponse, header)
import Data.Text (Text)
import Data.Aeson
import Data.String.Conversions
import Data.Text.Encoding

host :: Text
host = "https://discord.com/api"

runDiscordChannel :: (DiscordApiTokenReader :> es, Request :> es) => Eff (DiscordChannel : es) a -> Eff es a
runDiscordChannel = interpret $ \_ ->  \case
  SendMessage params  -> do
    let endpoint = host <> "/channel/" <> (convertString . show $ params.channelId) <> "/messages"
    token <- getToken
    _ <- request POST (https endpoint) (ReqBodyJson . toJSON $ params) ignoreResponse $
      header "Authorization" ("Bearer " <> encodeUtf8 token)
    pure ()
