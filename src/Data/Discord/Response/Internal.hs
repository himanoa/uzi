{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Discord.Response.Internal
  ( Response (..),
  )
where

import Data.Aeson
import Data.Aeson.Types
import Data.Discord.EventName
import Data.Discord.ReceiveEventOperationCode (ReceiveEventOperationCode)
import Data.Discord.ReceiveEventOperationCode qualified as OC
import Data.Discord.Response.HelloEventResponse
import Data.Discord.Response.MessageCreateEventResponse
import Data.Discord.Response.ReadyEventResponse
import Data.Functor
import Data.Discord.User

data Response = Hello HelloEventResponse | Ready ReadyEventResponse | MessageCreate MessageCreateEventResponse
  deriving (Show, Eq)

instance FromJSON Response where
  parseJSON = withObject "Response" $ \v ->
    v .: "op" >>= parseJSON @ReceiveEventOperationCode >>= \case
      OC.Hello -> pure . Hello $ HelloEventResponse
      OC.Ready ->
        v .: "t" >>= parseJSON @EventName >>= \case
          ReadyEventName -> do
            d <- v .: "d"
            userObj <- parseJSON @User =<< d .: "user"
            pure . Ready $ ReadyEventResponse { _user =  userObj }
          MessageCreateEventName -> parseJSON @MessageCreateEventResponse (Object v) <&> MessageCreate
          _ -> prependFailure "Not Supported" (typeMismatch "t" "xxx")
