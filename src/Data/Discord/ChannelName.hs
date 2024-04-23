{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Data.Discord.ChannelName where

import RIO
import Data.Aeson
import Data.Coerce
import Data.Text qualified as DT

newtype ChannelName = ChannelName DT.Text
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via DT.Text

coerceChannelName :: ChannelName -> DT.Text
coerceChannelName = coerce
