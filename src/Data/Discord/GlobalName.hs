{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Data.Discord.GlobalName where
import Data.Text
import Data.Aeson

newtype GlobalName = GlobalName Text
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via Text
