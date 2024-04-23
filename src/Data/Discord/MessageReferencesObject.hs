{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Data.Discord.MessageReferencesObject where

import RIO
import Data.Aeson
import Data.Discord.ChannelId
import Data.Text qualified as DT

data MessageReferencesObject = MessageReferencesObject
  { messageId :: DT.Text,
    channelId :: ChannelId,
    guildId :: DT.Text,
    failIfNotExist :: Bool
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON)
