{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Discord.Response.ReadyEventResponse where

import Data.Aeson
import Data.Aeson.Types
import Data.Discord.ReceiveEventOperationCode
import GHC.Generics
import Data.Discord.User 
import Control.Lens (makeLenses)

newtype ReadyEventResponse = ReadyEventResponse { _user :: User }
  deriving (Show, Generic, Eq)

makeLenses ''ReadyEventResponse

instance FromJSON ReadyEventResponse where
  parseJSON = withObject "HelloEventResponse" $ \v -> do
    operationCode <- parseJSON @ReceiveEventOperationCode =<< v .: "op"
    case operationCode of
      Ready -> do
        d <- v.: "d"
        userObj <- parseJSON @User =<< d.: "user"
        pure ReadyEventResponse { _user = userObj }
      _ -> prependFailure "Not supported op code" (typeMismatch "op" "")
