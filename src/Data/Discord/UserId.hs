{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Data.Discord.UserId where

import Data.Aeson
import Data.Text

newtype UserId = UserId Text
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via Text
