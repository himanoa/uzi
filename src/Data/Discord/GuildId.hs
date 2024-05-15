{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Data.Discord.GuildId where

import Data.Aeson
import RIO.Text qualified as DT
import RIO

newtype GuildId = GuildId DT.Text
  deriving (Eq)
  deriving (FromJSON, ToJSON, Show) via DT.Text
