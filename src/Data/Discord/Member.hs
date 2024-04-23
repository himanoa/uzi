{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Data.Discord.Member
  ( Role (..),
    Member (..),
  )
where

import RIO
import Data.Aeson
import Data.Discord.Nickname
import Data.Text qualified as DT

newtype Role = Role DT.Text
  deriving (Show, Eq)
  deriving (FromJSON) via DT.Text

data Member = Member
  { roles :: [Role],
    nick :: Maybe Nickname
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (FromJSON)
