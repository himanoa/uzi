{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

{-|
 Module: Data.Uzi.TimesChannel
 Description: 宇治共和国のDiscordに存在するtimesテキストチャンネルの定義
 Maintainer: himanoa <matsunoappy@gmail.com>

 宇治共和国のDiscordに存在するtimesテキストチャンネルの定義
-}

module Data.Uzi.TimesChannel where

import Control.Lens
import Data.Aeson
import Data.Coerce
import Data.Discord.Channel qualified as C
import Data.Discord.ChannelId
import Data.Discord.ChannelName
import Data.Either.Combinators (rightToMaybe)
import Data.Text
import RIO hiding ((^.))
import RIO.Vector qualified as RIOV
import Text.Parsec qualified as P
import Text.Parsec.Text qualified as P

-- | Timesチャンネルに使われている名前のデータ構造です。
--
-- 基本的な形は #times-{name} のnameの部分を表わしています。
--
-- だがもし、君が例外だというのなら…
-- ならば生き延びるがいい、君にはその権利と義務がある
--
-- 詳細は 'Data.Uzi.makeTimesChannel' を参照してください。
newtype TimesName = TimesName Text
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON, Ord) via Text

coerceTimesName :: TimesName -> Text
coerceTimesName = coerce

-- | Timesチャンネルを表すデータ構造です。
data TimesChannel = TimesChannel
  {
    -- | TimesChannelのDiscord上でのチャンネルID
    _id :: ChannelId, 
    -- | チャンネルの名前
    _name :: TimesName 
  }
  deriving (Show, Eq)

makeLenses ''TimesChannel

instance Ord TimesChannel where
  compare a b = compare (a ^. name) (b ^. name)

-- | DiscordのChannelからTimesChannel型に変換するためのスマートコンストラクタです
--
-- 変換に失敗する可能性があります。
-- 失敗要因は後述する TimesChannelの命名規則を満たさないチャンネルが引数に渡された場合です。
--
-- = TimesChannelの命名規則
--
-- TimesChannelは次の命名規則の中でいずれかを満たす必要があります
--
-- * BasicTimesName: #times-{name} の形式
-- * ShortTimesName: #time${name} の形式
-- * TheNyTimesName: #the-{name}-times の形式
--
makeTimesChannel ::
  -- | TimesChannelに変換したいDiscord上のChannel
  C.Channel 
  -- | 変換に成功した場合は'Just' 。失敗した場合は 'Nothing'
  -> Maybe TimesChannel 
makeTimesChannel c = case c ^. C._type of
  C.GuildVoice -> Nothing
  C.GuildCategory -> Nothing
  C.GuildText -> do
    let channelName = c ^. C._name
    let cid = c ^. C._id
    let timesNameMaybe = rightToMaybe (P.runParser timesNameParser () "TimesName" (coerce channelName))
    fmap (\n -> TimesChannel {_id = cid, _name = n}) timesNameMaybe
  where
    basicTimesNameParser :: P.Parser TimesName
    basicTimesNameParser = do
      _ <- P.string "times-"
      chars <- P.many1 P.anyChar
      pure . TimesName . pack $ chars

    shortTimesNameParser :: P.Parser TimesName
    shortTimesNameParser = do
      _ <- P.try (P.string "time" >> P.notFollowedBy (P.string "s-"))
      chars <- P.many1 P.anyChar
      pure . TimesName . pack $ chars

    theNyTimesParser :: P.Parser TimesName
    theNyTimesParser = do
      _ <- P.try . P.string $ "the-"
      chars <- P.many1 . P.noneOf $ "-"
      _ <- P.string "-times"
      pure . TimesName . pack $ chars

    timesNameParser :: P.Parser TimesName
    timesNameParser = theNyTimesParser P.<|> shortTimesNameParser P.<|> basicTimesNameParser

-- | 複数の 'Data.Discord.Channel' に対して変換処理を行い、成功したもののみを返す関数です
fromChannels :: RIO.Vector C.Channel -> RIO.Vector TimesChannel
fromChannels cs = RIOV.fromList $ RIO.catMaybes $ fmap makeTimesChannel (RIO.toList cs)
