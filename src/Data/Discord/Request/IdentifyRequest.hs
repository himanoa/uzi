{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Data.Discord.Request.IdentifyRequest where

import Data.Aeson (ToJSON)
import Data.Bits
import Data.Text
import GHC.Generics (Generic)

messageContentIntent :: Integer
messageContentIntent = shift 1 15

guildMessageContentIntent :: Integer
guildMessageContentIntent = shift 1 9

data IdentifyRequestProperties = IdentifyRequestProperties
  { os :: String,
    browser :: String,
    device :: String
  }
  deriving (Show, Generic, Eq)
  deriving anyclass (ToJSON)

data IdentifyRequest = IdentifyRequest
  { token :: Text,
    intents :: Integer,
    properties :: IdentifyRequestProperties
  }
  deriving (Show, Generic, Eq)
  deriving anyclass (ToJSON)

defaultIdentifyRequestProperties :: IdentifyRequestProperties
defaultIdentifyRequestProperties =
  IdentifyRequestProperties
    { os = "Linux",
      browser = "browser",
      device = "server"
    }

defaultIntents :: Integer
defaultIntents = messageContentIntent .|. guildMessageContentIntent

defaultIdentifyRequest :: Text -> IdentifyRequest
defaultIdentifyRequest token =
  IdentifyRequest
    { token = token,
      intents = defaultIntents,
      properties = defaultIdentifyRequestProperties
    }
