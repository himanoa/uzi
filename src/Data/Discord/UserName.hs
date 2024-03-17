{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Data.Discord.UserName where

import Data.Aeson
import Data.Text

newtype UserName = UserName Text
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via Text
