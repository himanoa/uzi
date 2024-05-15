{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Data.Discord.Member
  ( Role (..),
    Member (..),
  )
where

import Data.Aeson
import Data.Discord.Nickname
import RIO.Text qualified as T
import RIO

newtype Role = Role T.Text
  deriving (Show, Eq)
  deriving (FromJSON) via T.Text

data Member = Member
  { roles :: [Role],
    nick :: Maybe Nickname
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (FromJSON)
