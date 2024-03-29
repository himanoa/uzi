{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Data.Discord.GlobalName where

import Data.Aeson
import Data.Text

newtype GlobalName = GlobalName Text
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via Text
