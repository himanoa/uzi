{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Data.Discord.GuildId where

import Data.Aeson
import RIO.Text qualified as T
import RIO

newtype GuildId = GuildId T.Text
  deriving (Eq)
  deriving (FromJSON, ToJSON, Show) via T.Text
