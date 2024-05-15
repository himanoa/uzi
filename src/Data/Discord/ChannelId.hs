{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Data.Discord.ChannelId where

import Data.Aeson
import Data.Coerce
import RIO.Text qualified as DT
import RIO

newtype ChannelId = ChannelId DT.Text
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via DT.Text

coerceChannelId :: ChannelId -> DT.Text
coerceChannelId = coerce
