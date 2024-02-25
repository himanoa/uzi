{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Data.Discord.Nickname where

import Data.Aeson
import Data.Text

newtype Nickname = Nickname Text
  deriving (Show, Eq)
  deriving FromJSON via Text
