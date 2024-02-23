{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}

module Data.Discord.Request.Identify where
import GHC.Generics (Generic)
import Data.Aeson (ToJSON)

data IdentifyRequestProperties = IdentifyRequestProperties {
  os :: String,
  browser :: String,
  device :: String
}
  deriving (Show, Generic)
  deriving anyclass (ToJSON)

data IdentifyRequest  = IdentifyRequest {
  token :: String,
  intents :: Integer,
  properties :: IdentifyRequestProperties
}
  deriving (Show, Generic)
  deriving anyclass (ToJSON)

defaultIdentifyRequestProperties :: IdentifyRequestProperties
defaultIdentifyRequestProperties = IdentifyRequestProperties {
  os  =  "Linux",
  browser = "browser",
  device = "server"
}

defaultIntents :: Integer
defaultIntents = 3243773

defaultIdentifyRequest :: String -> IdentifyRequest

defaultIdentifyRequest token = IdentifyRequest {
  token = token,
  intents = defaultIntents,
  properties = defaultIdentifyRequestProperties
}
