{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Data.Discord.GuildId where

import Data.Aeson
import Data.Text

newtype GuildId = GuildId Text
  deriving (Eq)
  deriving (FromJSON, ToJSON, Show) via Text
