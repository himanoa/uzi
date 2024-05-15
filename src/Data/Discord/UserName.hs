{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Data.Discord.UserName where

import Data.Aeson
import RIO
import RIO.Text qualified as T

newtype UserName = UserName T.Text
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via T.Text
