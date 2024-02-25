{-# LANGUAGE OverloadedStrings #-}

module Data.Discord.Response.Internal
  ( Response (..),
  )
where

import Data.Aeson
import Data.Discord.Response.ReadyEventResponse
import Data.Discord.Response.HelloEventResponse 
import Data.Discord.ReceiveEventOperationCode qualified as OC
import Data.Discord.ReceiveEventOperationCode (ReceiveEventOperationCode)
import Data.Discord.EventName
import Data.Aeson.Types

data Response = Hello HelloEventResponse | Ready ReadyEventResponse
  deriving (Show, Eq)

instance FromJSON Response where
  parseJSON = withObject "Response" $ \v -> do
    code <- parseJSON @ReceiveEventOperationCode =<< v .: "op"
    case code of
      OC.Hello -> pure . Hello $ HelloEventResponse
      OC.Ready -> do
        t <- parseJSON @EventName =<< v .: "t"
        case t of 
          ReadyEventName -> pure . Ready $ ReadyEventResponse
          _ -> prependFailure "Not Supported" (typeMismatch "t" "xxx" )

