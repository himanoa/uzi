{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE DerivingVia #-}

module Data.Uzi.TimesChannel where 
import Data.Text
import Data.Aeson
import Data.Discord

newtype TimesName = TimesName Text
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON, Ord) via Text

data TimesChannel  = TimesChannel {
  _id :: ChannelId,
  _name :: TimesName
}
  deriving (Show, Eq)

instance Ord TimesChannel where 
  compare a b = compare (_name a) (_name b)
