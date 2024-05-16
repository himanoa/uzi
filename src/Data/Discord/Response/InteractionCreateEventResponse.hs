{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Discord.Response.InteractionCreateEventResponse
  ( InteractionCreateEventResponse (..),
    InteractionId (..),
    InteractionToken (..),
    channelId,
    member,
    guildId,
    slashCommandName,
    commandOptions,
    makeInteractionCreateEventResponse,
    interactionId,
    token,
  )
where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Types
import Data.Discord.ChannelId
import Data.Discord.GuildId (GuildId)
import Data.Discord.Member
import Data.Map.Strict qualified as M
import RIO

data OptionPair = OptionPair !Text !Text
  deriving (Eq, Show)

newtype InteractionId = InteractionId Text
  deriving (Eq)
  deriving (FromJSON, ToJSON, Show) via Text

newtype InteractionToken = InteractionToken Text
  deriving (Eq)
  deriving (FromJSON, ToJSON, Show) via Text

instance FromJSON OptionPair where
  parseJSON = withObject "OptionPair" $ \x -> do
    String key <- x .: "name"
    String value <- x .: "value"
    pure (OptionPair key value)

data InteractionCreateEventResponse = InteractionCreateEventResponse
  { _channelId :: ChannelId,
    _member :: Member,
    _slashCommandName :: Text,
    _commandOptions :: Map Text Text,
    _guildId :: GuildId,
    _interactionId :: InteractionId,
    _token :: InteractionToken
  }
  deriving (Show, Eq)

makeLenses ''InteractionCreateEventResponse

makeInteractionCreateEventResponse :: ChannelId -> Member -> Text -> Map Text Text -> GuildId -> InteractionId -> InteractionToken -> InteractionCreateEventResponse
makeInteractionCreateEventResponse = InteractionCreateEventResponse

instance FromJSON InteractionCreateEventResponse where
  parseJSON :: Value -> Parser InteractionCreateEventResponse
  parseJSON = withObject "InteractionCreateEventResponse" $ \o -> do
    dataSection <- o .: "d"
    _channelId <- parseJSON @ChannelId =<< dataSection .: "channel_id"
    _member <- parseJSON @Member =<< dataSection .: "member"
    _guildId <- parseJSON @GuildId =<< dataSection .: "guild_id"

    _interactionId <- parseJSON @InteractionId =<< dataSection .: "id"
    _token <- parseJSON @InteractionToken =<< dataSection .: "token"

    __data <- dataSection .: "data"
    _slashCommandName <- parseJSON @Text =<< __data .: "name"

    _commandOptions <- do
      maybeOpts <- __data .:? "options"
      case maybeOpts of
        Just opts -> parseJSON @[OptionPair] opts <&> M.fromList . map (\(OptionPair x y) -> (x, y))
        Nothing -> pure M.empty

    pure InteractionCreateEventResponse {..}
