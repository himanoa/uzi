{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Data.Discord.MessageReferencesObject where

import Data.Text
import GHC.Generics
import Data.Aeson

data MessageReferencesObject = MessageReferencesObject {
  messageId :: Text,
  channelId :: Text,
  guildId :: Text,
  failIfNotExist :: Bool
}
  deriving (Show,Eq, Generic)
  deriving ToJSON
