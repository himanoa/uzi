{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE DeriveAnyClass #-}
module Data.Discord.Member (
  Role(..),
  Member(..)
)where

import Data.Aeson
import Data.Text
import GHC.Generics 
import Data.Discord.Nickname 

newtype Role = Role Text
  deriving (Show, Eq)
  deriving FromJSON via Text


data Member = Member {
  roles :: [Role],
  nick :: Nickname
}
  deriving (Show, Eq, Generic)
  deriving anyclass FromJSON
