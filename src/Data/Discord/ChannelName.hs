{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Data.Discord.ChannelName where

import Data.Aeson
import Data.Coerce
import RIO.Text qualified as T
import RIO

newtype ChannelName = ChannelName T.Text
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via T.Text

coerceChannelName :: ChannelName -> T.Text
coerceChannelName = coerce
