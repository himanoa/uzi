{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Data.Discord.UserId where
import Data.Text
import Data.Aeson

newtype UserId = UserId Text
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via Text
