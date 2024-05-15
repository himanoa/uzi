{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Data.Discord.GlobalName where

import Data.Aeson
import RIO.Text qualified as T
import RIO

newtype GlobalName = GlobalName T.Text
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via T.Text
