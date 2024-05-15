{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Data.Discord.UserId where

import Data.Aeson
import RIO.Text qualified as T
import RIO

newtype UserId = UserId T.Text
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via T.Text
