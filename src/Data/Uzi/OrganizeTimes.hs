{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

-- |
-- Module: Data.Uzi.OrganizeTimes
-- Description: TimesChannelの整理を行うモジュール
-- Maintainer: himanoa <matsunoappy@gmail.com>
--
-- 宇治共和国では TIMES(A-M) TIMES(A-N) といったグループチャンネルが存在し、各timesの頭文字にを該当するグループチャンネルに紐付けて管理しています。
-- このモジュールはそれらの紐付けと並び換えを自動化するための定義です
-- |
module Data.Uzi.OrganizeTimes where

import Data.Aeson
import Data.Discord hiding (coerceChannelId)
import Data.Discord.Channel
import Data.Discord.Channel qualified as C
import Data.Uzi.TimesChannel qualified as TC
import Data.Uzi.TimesChannelGroup
import Control.Lens
import Effectful
import Effectful.DiscordChannel
import Effectful.DiscordChannel.Effect (getChannels)
import Effectful.DynamicLogger (DynamicLogger)
import Effectful.DynamicLogger.Effect (info)
import Effectful.Error.Dynamic
import Effectful.State.Static.Local
import RIO hiding ((^.))
import RIO.Map qualified as M
import RIO.Vector qualified as V
import RIO.Vector.Boxed qualified as VU

--

-- | 'Data.Uzi.OrganizeTimes.organizeTimes' を実行した時に発生しうるエラーをまとめたデータ構造です。
-- 発生しうるエラーとシチュエーションは次の通りです
--
-- 'Data.Uzi.OrganizeTimes.FindTimesError': TIMES(A-M) TIMES(N-Z) という名前のグループチャンネルがGuildIdが指すサーバーにどちらか片方でも存在しない場合に発生します
-- 'Data.Uzi.OrganizeTimes.FindChannelError: ソートしたあとに実際にDiscordに変更を反映する過程でTimesChannelに記載されているChannelIdが存在しない場合に発生します
data OrganizeTimesError = FindTimesError FindTimesChannelGroupsError | FindChannelError ChannelId
  deriving (Show, Eq)

--

-- | 実際にTimesChannelを整理する関数です。
--
-- 整理とは次の行為を指します
--
-- * サーバーに存在する全てのTimesChannelを、引数に渡されたグループチャンネルに適切にグルーピングする行為。詳細は 'Data.Uzi.TimesChannelGroup.groupByFirstLetter' を参照してください
-- * グループ内のtimesの頭文字をアルファベットで昇順ソートする行為
organizeTimes ::
  ( DiscordChannel :> es,
    Error OrganizeTimesError :> es,
    DynamicLogger :> es
  ) =>
  -- | TimesChannelの整理を行うDiscordサーバーの 'Data.Discord.GuildId'
  GuildId ->
  Eff es ()
organizeTimes guildId = do
  channels <- getChannels guildId
  let channelsVector = V.fromList channels :: VU.Vector C.Channel

  RIO.void $ info $ RIO.displayShow ("channelsVector: " <> encode channelsVector)

  (aToM, nToZ) <- case findTimesCategories channelsVector of
    Right a -> pure a
    Left e -> throwError . FindTimesError $ e

  let channelsMap = groupByFirstLetter (TC.fromChannels channelsVector) aToM nToZ
  let sortedChannelsMap = sortTimesChannelGroupMap channelsMap

  RIO.void $ M.traverseWithKey (updateChannelPositions channelsVector) sortedChannelsMap
  where
    updateChannelPositions :: (DiscordChannel :> es, Error OrganizeTimesError :> es) => Vector Channel -> TimesChannelGroup -> [TC.TimesChannel] -> Eff es ()
    updateChannelPositions channels group times = do
      RIO.void $ evalState @Integer 1 (traverse (updateChannelPosition channels (coerceChannelId group)) times)

    updateChannelPosition :: (DiscordChannel :> es, State Integer :> es, Error OrganizeTimesError :> es) => Vector Channel -> ChannelId -> TC.TimesChannel -> Eff es ()
    updateChannelPosition channels parentId tc = do
      count <- get
      channel <- maybe (throwError . FindChannelError $ tc ^. TC.id) pure (V.find (\c -> (c ^. C._id) == (tc ^. TC.id)) channels)
      RIO.void $ modifyChannel channel
      RIO.void $ put (count + 1)
    changePosition :: Channel ->  ChannelId ->  ChannelPosition -> Channel
    changePosition channel channelId position = undefined
      -- let updatedParentId = set' (channel ^. C._)
