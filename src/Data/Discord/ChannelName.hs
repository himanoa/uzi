{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Data.Discord.ChannelName  where
import Data.Text
import Data.Aeson

newtype ChannelName = ChannelName Text
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via Text

