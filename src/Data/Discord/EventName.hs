{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Data.Discord.EventName where

import Data.Aeson
import Data.Aeson.Types (prependFailure, typeMismatch)
import RIO

data EventName = ReadyEventName | MessageCreateEventName | GuildCreateEventName | InteractionCreateEventName
  deriving (Show, Eq)

instance FromJSON EventName where
  parseJSON = withText "EventName" $ \case
    "READY" -> pure ReadyEventName
    "MESSAGE_CREATE" -> pure MessageCreateEventName
    "GUILD_CREATE" -> pure GuildCreateEventName
    "INTERACTION_CREATE" -> pure InteractionCreateEventName
    t -> prependFailure ("Not supported op code " <> show t) (typeMismatch "Opcode" (String t))
