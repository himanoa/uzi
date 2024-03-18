{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Data.Uzi.TimesChannelGroup where
import Data.Discord (ChannelId)

data TimesChannelGroup = AtoMGroup ChannelId | NtoZGroup ChannelId
  deriving (Show, Eq)

