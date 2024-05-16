{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Data.Discord.ChannelId where

import Data.Aeson
import Data.Coerce
import RIO
import RIO.Text qualified as T

newtype ChannelId = ChannelId T.Text
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via T.Text

coerceChannelId :: ChannelId -> T.Text
coerceChannelId = coerce

makeChannelId :: T.Text -> ChannelId
makeChannelId = ChannelId
