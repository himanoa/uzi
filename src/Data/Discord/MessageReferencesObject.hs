{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Data.Discord.MessageReferencesObject where

import Data.Aeson
import Data.Discord.ChannelId
import RIO.Text qualified as T
import RIO

data MessageReferencesObject = MessageReferencesObject
  { messageId :: T.Text,
    channelId :: ChannelId,
    guildId :: T.Text,
    failIfNotExist :: Bool
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON)
