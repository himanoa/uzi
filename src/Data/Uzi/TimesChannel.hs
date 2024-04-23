{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Data.Uzi.TimesChannel where

import RIO hiding ((^.)) 
import RIO.Vector qualified as RIOV
import Control.Lens
import Data.Aeson
import Data.Coerce
import Data.Discord.Channel qualified as C
import Data.Discord.ChannelId
import Data.Discord.ChannelName
import Data.Either.Combinators (rightToMaybe)
import Data.Text
import Text.Parsec qualified as P
import Text.Parsec.Text qualified as P

newtype TimesName = TimesName Text
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON, Ord) via Text

coerceTimesName :: TimesName -> Text
coerceTimesName = coerce

data TimesChannel = TimesChannel
  { _id :: ChannelId,
    _name :: TimesName
  }
  deriving (Show, Eq)

makeLenses ''TimesChannel

instance Ord TimesChannel where
  compare a b = compare (a ^. name) (b ^. name)

makeTimesChannel :: C.Channel -> Maybe TimesChannel
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

fromChannels :: RIO.Vector C.Channel -> RIO.Vector TimesChannel
fromChannels cs = RIOV.fromList $ RIO.catMaybes $ fmap makeTimesChannel (RIO.toList cs)
