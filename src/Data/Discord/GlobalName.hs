{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Data.Discord.GlobalName where

import Data.Aeson
import Data.Text qualified as DT
import RIO

newtype GlobalName = GlobalName DT.Text
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via DT.Text
