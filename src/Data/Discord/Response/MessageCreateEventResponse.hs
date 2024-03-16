{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Data.Discord.Response.MessageCreateEventResponse
  ( MessageCreateEventResponse,
    channelId,
    content,
    mentions,
    member,
    isBot,
    makeMessageCreateEventResponse
  )
where

import Data.Aeson
import Data.Discord.Member
import Data.Discord.Content
import Data.Maybe (fromMaybe)
import Data.Discord.ChannelId
import Control.Lens

data MessageCreateEventResponse = MessageCreateEventResponse
  { _channelId :: ChannelId,
    _content :: Content,
    _mentions :: [Member],
    _member :: Member,
    _isBot :: Bool
  }
  deriving (Show, Eq)

makeLenses ''MessageCreateEventResponse

makeMessageCreateEventResponse :: ChannelId -> Content -> [Member] -> Member -> Bool -> MessageCreateEventResponse
makeMessageCreateEventResponse = MessageCreateEventResponse 
  
instance FromJSON MessageCreateEventResponse where
  parseJSON = withObject "MessageCreateEventResponse" $ \o -> do
    dataSection <- o .: "d"
    _channelId <- parseJSON @ChannelId =<< dataSection .: "channel_id"
    _content <- parseJSON @Content =<< dataSection .: "content"
    _mentions <- parseJSON @[Member] =<< dataSection .: "mentions"
    _member <- parseJSON @Member =<< dataSection .: "member"
    _author <- dataSection .: "author"
    _isBot <- fromMaybe False <$> _author .:? "bot"
    pure
      MessageCreateEventResponse
        {
          ..
        }
