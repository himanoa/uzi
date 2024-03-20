{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# LANGUAGE LambdaCase #-}

module Data.Uzi.TimesChannelGroup
  ( TimesChannelGroup (..),
    FindTimesChannelGroupsError (..),
    findTimesCategories,
    groupByFirstLetter,
    sortTimesChannelGroupMap,
    coerceChannelId
  )
where

import Control.Lens
import Data.Discord.Channel qualified as C
import Data.Discord.ChannelId qualified as C
import Data.Discord.ChannelName
import Data.Text qualified as Text
import Data.Uzi.TimesChannel qualified as TC
import RIO qualified
import RIO.Map qualified as Map
import RIO.Vector qualified as RIOV
import Data.List (sort)

data TimesChannelGroup = AtoMGroup C.ChannelId | NtoZGroup C.ChannelId
  deriving (Show, Eq)

coerceChannelId :: TimesChannelGroup -> C.ChannelId
coerceChannelId = \case
  AtoMGroup cid -> cid
  NtoZGroup cid -> cid

instance Ord TimesChannelGroup where
  compare a b = case (a, b) of
    (AtoMGroup _, NtoZGroup _) -> GT
    (NtoZGroup _, AtoMGroup _) -> LT
    _ -> EQ

data FindTimesChannelGroupsError = AtoMGroupMissing | NtoZGroupMissing | AllMissing
  deriving (Show, Eq)

-- Searches for TIMES (A-M) and TIMES (N-Z) channels from a Vector of all server channels and returns them.
findTimesCategories :: RIO.Vector C.Channel -> Either FindTimesChannelGroupsError (TimesChannelGroup, TimesChannelGroup)
findTimesCategories cs =
  case RIOV.foldr' findTimesChannelGroups (Nothing, Nothing) cs of
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

groupByFirstLetter :: RIO.Vector TC.TimesChannel -> TimesChannelGroup -> TimesChannelGroup -> RIO.Map TimesChannelGroup [TC.TimesChannel]
groupByFirstLetter channels aToM nToZ = do
  Map.fromListWith (++) $ RIO.catMaybes . RIOV.toList $ fmap (getGroupTuple aToM nToZ) channels
  where
    getGroupTuple :: TimesChannelGroup -> TimesChannelGroup -> TC.TimesChannel -> Maybe (TimesChannelGroup, [TC.TimesChannel])
    getGroupTuple aToMG nToZG c = do
      let prefix = Text.toLower . Text.take 1 . TC.coerceTimesName $ (c ^. TC.name)
      case prefix of
        "a" -> Just (aToMG, [c])
        "b" -> Just (aToMG, [c])
        "c" -> Just (aToMG, [c])
        "d" -> Just (aToMG, [c])
        "e" -> Just (aToMG, [c])
        "f" -> Just (aToMG, [c])
        "g" -> Just (aToMG, [c])
        "h" -> Just (aToMG, [c])
        "i" -> Just (aToMG, [c])
        "j" -> Just (aToMG, [c])
        "k" -> Just (aToMG, [c])
        "l" -> Just (aToMG, [c])
        "m" -> Just (aToMG, [c])
        "n" -> Just (nToZG, [c])
        "o" -> Just (nToZG, [c])
        "p" -> Just (nToZG, [c])
        "q" -> Just (nToZG, [c])
        "r" -> Just (nToZG, [c])
        "s" -> Just (nToZG, [c])
        "t" -> Just (nToZG, [c])
        "u" -> Just (nToZG, [c])
        "v" -> Just (nToZG, [c])
        "w" -> Just (nToZG, [c])
        "x" -> Just (nToZG, [c])
        "y" -> Just (nToZG, [c])
        "z" -> Just (nToZG, [c])
        _ -> Nothing

sortTimesChannelGroupMap :: RIO.Map TimesChannelGroup [TC.TimesChannel] -> RIO.Map TimesChannelGroup [TC.TimesChannel]
sortTimesChannelGroupMap = Map.map sort 
