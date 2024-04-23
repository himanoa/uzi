{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Data.Discord.UserName where

import RIO
import Data.Aeson
import Data.Text qualified as DT

newtype UserName = UserName DT.Text
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via DT.Text
