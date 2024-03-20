{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Data.Uzi.OrganizeTimes where

import Data.Discord hiding (coerceChannelId)
import Data.Discord.Channel
import Data.Discord.Channel qualified as C
import Data.Uzi.TimesChannel
import Data.Uzi.TimesChannelGroup
import Effectful
import Effectful.DiscordChannel
import Effectful.DiscordChannel.Effect (getChannels)
import Effectful.Error.Dynamic
import Effectful.State.Static.Local
import RIO.Map qualified as M
import RIO.Vector qualified as V
import RIO.Vector.Boxed qualified as VU
import RIO qualified

newtype OrganizeTimesError = FindTimesError FindTimesChannelGroupsError
  deriving (Show, Eq)

organizeTimes :: (DiscordChannel :> es, Error OrganizeTimesError :> es) => GuildId -> Eff es ()
organizeTimes guildId = do
  channels <- getChannels guildId
  let channelsVector = V.fromList channels :: VU.Vector C.Channel

  (aToM, nToZ) <- case findTimesCategories channelsVector of
    Right a -> pure a
    Left e -> throwError . FindTimesError $ e

  let channelsMap = groupByFirstLetter (fromChannels channelsVector) aToM nToZ
  let sortedChannelsMap = sortTimesChannelGroupMap channelsMap

  RIO.void $ M.traverseWithKey updateChannelPositions sortedChannelsMap
  where
    updateChannelPositions :: (DiscordChannel :> es) => TimesChannelGroup -> [TimesChannel] -> Eff es ()
    updateChannelPositions group channels = do
      RIO.void $ evalState @Integer 1 (traverse (updateChannelPosition (coerceChannelId group)) channels)

    updateChannelPosition :: (DiscordChannel :> es, State Integer :> es) => ChannelId -> TimesChannel -> Eff es ()
    updateChannelPosition parentId tc = do
      count <- get
      RIO.void $ modifyChannel guildId parentId tc (ChannelPosition count)
      RIO.void $ put (count + 1)
