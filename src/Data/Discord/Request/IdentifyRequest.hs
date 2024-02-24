{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Data.Discord.Request.IdentifyRequest where

import Data.Aeson (ToJSON)
import Data.Text
import GHC.Generics (Generic)

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
defaultIntents = 3243773

defaultIdentifyRequest :: Text -> IdentifyRequest
defaultIdentifyRequest token =
  IdentifyRequest
    { token = token,
      intents = defaultIntents,
      properties = defaultIdentifyRequestProperties
    }
