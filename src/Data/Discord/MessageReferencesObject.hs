{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Data.Discord.MessageReferencesObject where

import Data.Aeson
import Data.Discord.ChannelId
import Data.Text
import GHC.Generics

data MessageReferencesObject = MessageReferencesObject
  { messageId :: Text,
    channelId :: ChannelId,
    guildId :: Text,
    failIfNotExist :: Bool
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON)
