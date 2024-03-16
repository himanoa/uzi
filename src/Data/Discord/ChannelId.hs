{-# LANGUAGE DerivingVia #-}

module Data.Discord.ChannelId where

import Data.Aeson
import Data.Text

newtype ChannelId = ChannelId Text
  deriving (Eq)
  deriving (FromJSON, ToJSON, Show) via Text
