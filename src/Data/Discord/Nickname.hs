{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Data.Discord.Nickname where

import Data.Aeson
import RIO
import RIO.Text qualified as T

newtype Nickname = Nickname T.Text
  deriving (Show, Eq)
  deriving (FromJSON) via T.Text
