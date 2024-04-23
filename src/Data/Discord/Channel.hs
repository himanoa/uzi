{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Data.Discord.Channel where

import Control.Lens (makeLenses)
import Data.Aeson
import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.Coerce
import Data.Discord.ChannelId
import Data.Discord.ChannelName
import Data.Scientific
import RIO

data ChannelType = GuildText | GuildVoice | GuildCategory
  deriving (Show, Eq, Generic)

instance FromJSON ChannelType where
  parseJSON = withScientific "ChannelType" $ \v -> case toBoundedInteger @Int v of
    Just 0 -> pure GuildText
    Just 2 -> pure GuildVoice
    Just 4 -> pure GuildCategory
    value -> prependFailure ("Not supported type " <> show value) (typeMismatch "Opcode" (Number v))

instance ToJSON ChannelType where
  toJSON = \case
    GuildText -> Number 0
    GuildVoice -> Number 2
    GuildCategory -> Number 4

newtype ChannelPosition = ChannelPosition Integer
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via Integer

coerceChannelPosition :: ChannelPosition -> Integer
coerceChannelPosition = coerce

channelTypeFromInteger :: Int -> Maybe ChannelType
channelTypeFromInteger 0 = Just GuildText
channelTypeFromInteger 4 = Just GuildCategory
channelTypeFromInteger _ = Nothing

data Channel = Channel
  { __id :: ChannelId,
    __type :: ChannelType,
    __position :: ChannelPosition,
    __name :: ChannelName
  }
  deriving (Show, Eq, Generic)

instance FromJSON Channel where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 2}

instance ToJSON Channel where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 2}

makeLenses ''Channel
