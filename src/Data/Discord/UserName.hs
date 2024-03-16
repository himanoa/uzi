{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Data.Discord.UserName where 

import Data.Text
import Data.Aeson

newtype UserName = UserName Text
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via Text
