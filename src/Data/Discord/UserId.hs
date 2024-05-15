{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Data.Discord.UserId where

import Data.Aeson
import RIO.Text qualified as DT
import RIO

newtype UserId = UserId DT.Text
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via DT.Text
