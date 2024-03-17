{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Data.Discord.ChannelName where

import Data.Aeson
import Data.Text

newtype ChannelName = ChannelName Text
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via Text
