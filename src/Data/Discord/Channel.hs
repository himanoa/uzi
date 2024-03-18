{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE LambdaCase #-}

module Data.Discord.Channel where

import Data.Text hiding (drop)
import Data.Aeson
import Data.Discord.ChannelName
import Data.Scientific
import Control.Lens (makeLenses)
import Data.Aeson.Types (prependFailure, typeMismatch)
import GHC.Generics

newtype ChannelId = ChannelId Text
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via Text

data ChannelType = GuildText |  GuildCategory
  deriving (Show, Eq, Generic)

instance FromJSON ChannelType where
  parseJSON = withScientific "ChannelType" $ \v -> case toBoundedInteger @Int v of
    Just 0 -> pure GuildText
    Just 4 -> pure GuildCategory
    _ -> prependFailure "Not supported op code " (typeMismatch "Opcode" (Number v))

instance ToJSON ChannelType where
  toJSON  = \case 
    GuildText -> Number 0
    GuildCategory -> Number 4

newtype ChannelPosition =  ChannelPosition Integer
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via Integer

channelTypeFromInteger :: Int -> Maybe ChannelType
channelTypeFromInteger 0  = Just GuildText
channelTypeFromInteger 4  = Just GuildCategory
channelTypeFromInteger _  = Nothing

data Channel = Channel {
  __id :: ChannelId,
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

