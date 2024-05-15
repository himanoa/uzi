{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Data.Discord.ChannelName where

import Data.Aeson
import Data.Coerce
import RIO.Text qualified as DT
import RIO

newtype ChannelName = ChannelName DT.Text
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via DT.Text

coerceChannelName :: ChannelName -> DT.Text
coerceChannelName = coerce
