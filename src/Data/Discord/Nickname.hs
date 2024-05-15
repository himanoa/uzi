{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Data.Discord.Nickname where

import Data.Aeson
import RIO.Text qualified as DT
import RIO

newtype Nickname = Nickname DT.Text
  deriving (Show, Eq)
  deriving (FromJSON) via DT.Text
