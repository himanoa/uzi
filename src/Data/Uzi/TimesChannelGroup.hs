{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Data.Uzi.TimesChannelGroup
  ( TimesChannelGroup (..),
    FindTimesChannelGroupsError (..),
    findTimesCategories,
  )
where

import Control.Lens
import Data.Discord.Channel qualified as C
import Data.Discord.ChannelName
import RIO qualified
import RIO.Vector qualified as RIOV

data TimesChannelGroup = AtoMGroup C.ChannelId | NtoZGroup C.ChannelId
  deriving (Show, Eq)

data FindTimesChannelGroupsError = AtoMGroupMissing | NtoZGroupMissing | AllMissing
  deriving (Show, Eq)

-- Searches for TIMES (A-M) and TIMES (N-Z) channels from a Vector of all server channels and returns them.
findTimesCategories :: RIO.Vector C.Channel -> Either FindTimesChannelGroupsError (TimesChannelGroup, TimesChannelGroup)
findTimesCategories cs =
  case RIOV.foldr findTimesChannelGroups (Nothing, Nothing) cs of
    (Just aToM, Just nToZ) -> Right (aToM, nToZ)
    (Just _, Nothing) -> Left NtoZGroupMissing
    (Nothing, Just _) -> Left AtoMGroupMissing
    (Nothing, Nothing) -> Left AllMissing
  where
    findTimesChannelGroups :: C.Channel -> (Maybe TimesChannelGroup, Maybe TimesChannelGroup) -> (Maybe TimesChannelGroup, Maybe TimesChannelGroup)
    findTimesChannelGroups c (aToM, nToZ) =
      if c ^. C._type == C.GuildCategory
        then case c ^. C._name of
          ChannelName "TIMES(A-M)" -> (Just (AtoMGroup (c ^. C._id)), nToZ)
          ChannelName "TIMES(N-Z)" -> (aToM, Just (NtoZGroup (c ^. C._id)))
          _ -> (aToM, nToZ)
        else (aToM, nToZ)
