{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

-- |
-- Module: Data.Uzi.TimesChannelGroup
-- Description: 宇治共和国のDiscordに存在するtimesチャンネルをグルーピングしたチャンネルの定義
-- Maintainer: himanoa <matsunoappy@gmail.com>
--
-- 宇治共和国のDiscordに存在するtimesチャンネルをグルーピングしたチャンネルの定義です。
--
-- 具体的には名前が TIMES(A-M) TIMES(N-Z) なグループチャンネルのことを指します
module Data.Uzi.TimesChannelGroup
  ( TimesChannelGroup (..),
    FindTimesChannelGroupsError (..),
    findTimesCategories,
    groupByFirstLetter,
    sortTimesChannelGroupMap,
    coerceChannelId,
  )
where

import Control.Lens
import Data.Discord.Channel qualified as C
import Data.Discord.ChannelId qualified as C
import Data.Discord.ChannelName
import Data.List (sort)
import RIO.Text qualified as T
import Data.Uzi.TimesChannel qualified as TC
import RIO hiding ((^.))
import RIO.Map qualified as Map
import RIO.Vector qualified as RIOV

-- | 宇治共和国に存在するTIMES(A-M) TIMES(N-Z) な名前のグループチャンネルのデータ定義
--
-- * 'Data.Uzi.TimesChannelGroup.AtoMGroup' は頭文字をA-MのTimesChannelをまとめたグループ
-- * 'Data.Uzi.TimesChannelGroup.NtoZGroup' は頭文字をN-ZのTimesChannelをまとめたグループ
data TimesChannelGroup = AtoMGroup C.ChannelId | NtoZGroup C.ChannelId
  deriving (Show, Eq)

coerceChannelId :: TimesChannelGroup -> C.ChannelId
coerceChannelId = \case
  AtoMGroup cid -> cid
  NtoZGroup cid -> cid

-- | ソートした場合は AtoM NtoZの順番になります
instance Ord TimesChannelGroup where
  compare a b = case (a, b) of
    (AtoMGroup _, NtoZGroup _) -> GT
    (NtoZGroup _, AtoMGroup _) -> LT
    _ -> EQ

-- | 'Data.Uzi.TimesChannelGroup.findTimesCategories' を実行した時に発生しうるエラーをまとめたデータ構造です。
--
-- 発生しうるエラーとシチュエーションは次の通りです
--
-- * 'Data.Uzi.TimesChannelGroup.AtoMGroupMissing' 引数で渡されたチャンネル一覧の中に AtoMになりうるグループチャンネルが存在しなかった場合
-- * 'Data.Uzi.TimesChannelGroup.NtoZGroupMissing' 引数で渡されたチャンネル一覧の中に NtoZになりうるグループチャンネルが存在しなかった場合
-- * 'Data.Uzi.TimesChannelGroup.AllMissing' 引数で渡されたチャンネル一覧の中に、AtoM,NtoZどちらも存在しなかった場合
data FindTimesChannelGroupsError = AtoMGroupMissing | NtoZGroupMissing | AllMissing
  deriving (Show, Eq)

-- | チャンネル一覧からTIMES(A-M)  TIMES(N-Z)のグループチャンネルを探し出して返す関数
--
-- この関数は失敗する可能性があります。
--
-- 失敗した場合に発生するエラーについては 'Data.Uzi.TimesChannelGroup.FindTimesChannelGroupsError'を参照してください。
findTimesCategories ::
  -- | チャンネルの一覧
  RIO.Vector C.Channel ->
  -- | 失敗した場合はLeft。成功した場合はRight(AtoMGroup, NtoZGroup)
  Either
    FindTimesChannelGroupsError
    ( TimesChannelGroup,
      TimesChannelGroup
    )
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
        then -- Disocrdはグループチャンネルを入力した時times(a-z)のような小文字で構成されたがグループチャンネル名だったとしても、表示上はTIMES(A-Z)にしてくる。
        -- しかし、データ上はtimes(a-z)として返ってくる
        -- これだとパターンマッチ時に使っている名前を一意に特定しにくいため、この実装ではチャンネル名は全て大文字として扱う
        case upperedChannelName c of
          ChannelName "TIMES(A-M)" -> (Just (AtoMGroup (c ^. C._id)), nToZ)
          ChannelName "TIMES(N-Z)" -> (aToM, Just (NtoZGroup (c ^. C._id)))
          _ -> (aToM, nToZ)
        else (aToM, nToZ)
    upperedChannelName :: C.Channel -> ChannelName
    upperedChannelName c = ChannelName . T.toUpper . coerceChannelName $ (c ^. C._name)

--

-- | timesチャンネルの名前の頭文字を使ってAtoM,NtoZにグルーピングする関数
--
-- グルーピングされたTimesChannelの順番がアルファベット順なことは保証されない
--
-- ソートしたい場合は戻り値に対して 'Data.Uzi.TimesChannelGroup.sortTimesChannelGroupMap' を呼び出してください
groupByFirstLetter ::
  -- | サーバーに存在する全てのTimesChannel
  RIO.Vector TC.TimesChannel ->
  -- | AtoMChannelGroup
  TimesChannelGroup ->
  -- | NtoZChannelGroup
  TimesChannelGroup ->
  -- | 'RIO.Map' によって表現されたグループとチャンネルの関連
  RIO.Map TimesChannelGroup [TC.TimesChannel]
groupByFirstLetter channels aToM nToZ = do
  Map.fromListWith (++) $ RIO.catMaybes . RIOV.toList $ fmap (getGroupTuple aToM nToZ) channels
  where
    getGroupTuple :: TimesChannelGroup -> TimesChannelGroup -> TC.TimesChannel -> Maybe (TimesChannelGroup, [TC.TimesChannel])
    getGroupTuple aToMG nToZG c = do
      let prefix = T.toLower . T.take 1 . TC.coerceTimesName $ (c ^. TC.name)
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

-- | 'Data.Uzi.TimesChannelGroup.groupByFirstLetter' の戻り値で関連付けられたTimesChannelのリストをソートする
sortTimesChannelGroupMap ::
  -- | 'RIO.Map' によって表現されたグループとチャンネルの関連
  RIO.Map TimesChannelGroup [TC.TimesChannel] ->
  -- | ソート済みのグループとチャンネルの関連
  RIO.Map TimesChannelGroup [TC.TimesChannel]
sortTimesChannelGroupMap = Map.map sort
