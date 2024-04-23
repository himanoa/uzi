{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Data.Discord.Response.ReadyEventResponse where

import RIO
import Control.Lens (makeLenses)
import Data.Aeson
import Data.Aeson.Types
import Data.Discord.ReceiveEventOperationCode
import Data.Discord.User

newtype ReadyEventResponse = ReadyEventResponse {_user :: User}
  deriving (Show, Generic, Eq)

makeLenses ''ReadyEventResponse

instance FromJSON ReadyEventResponse where
  parseJSON = withObject "HelloEventResponse" $ \v -> do
    operationCode <- parseJSON @ReceiveEventOperationCode =<< v .: "op"
    case operationCode of
      Ready -> do
        d <- v .: "d"
        userObj <- parseJSON @User =<< d .: "user"
        pure ReadyEventResponse {_user = userObj}
      _ -> prependFailure "Not supported op code" (typeMismatch "op" "")
