{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE BlockArguments #-}
module Data.Uzi.OrganizeTimes where

import Effectful
import Effectful.DiscordChannel
import Effectful.DynamicLogger
import Effectful.DiscordChannel.Effect (getChannels)
import Data.Discord
import Data.Discord.Channel qualified as C
import RIO.Vector qualified as V
import RIO.Vector.Boxed qualified as VU
import RIO.Map qualified as M
import Data.Uzi.TimesChannelGroup
import Data.Uzi.TimesChannel
import Effectful.State.Static.Local
import Data.Discord.Channel
import Effectful.Error.Dynamic

newtype OrganizeTimesError = FindTimesError FindTimesChannelGroupsError
  deriving (Show, Eq)

organizeTimes :: (DiscordChannel :> es, Error OrganizeTimesError :> es) => GuildId ->  Eff es ()
organizeTimes guildId = do
  channels <- getChannels guildId
  let channelsVector = V.fromList channels :: VU.Vector C.Channel

  (aToM, nToZ) <- case findTimesCategories channelsVector of
    Right a -> pure a
    Left e -> throwError . FindTimesError $ e

  let channelsMap =  groupByFirstLetter (fromChannels channelsVector) aToM nToZ
  let sortedChannelsMap = sortTimesChannelGroupMap channelsMap

  _ <- M.traverseWithKey updateChannelPositions sortedChannelsMap
  pure ()

  where
    updateChannelPositions :: (DiscordChannel :> es) => TimesChannelGroup -> [TimesChannel] -> Eff es ()
    updateChannelPositions group channels =  do
      _  <- evalState @Integer 1 (traverse (updateChannelPosition (coerceChannelId group) ) channels)
      pure ()

    updateChannelPosition :: (DiscordChannel :> es, State Integer :> es) => ChannelId -> TimesChannel -> Eff es ()
    updateChannelPosition parentId tc = do
      count <- get
      _ <- modifyChannel guildId  parentId tc (ChannelPosition count)
      _ <- put (count + 1)
      pure ()
