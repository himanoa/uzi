{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Data.Discord.ChannelId where

import Data.Aeson
import Data.Text

newtype ChannelId = ChannelId Text
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via Text
