{-# LANGUAGE DerivingVia #-}
module Data.Discord.Response.MessageCreateEventResponse where
import Data.Text
import Data.Aeson

newtype ChannelId = ChannelId Text
  deriving (Show, Eq)
  deriving FromJSON via Text

newtype Content = Content Text
  deriving (Show, Eq)
  deriving FromJSON via Text

data MessageCreateEventResponse = MessageCreateEventResponse {
  channelId :: ChannelId,
  content :: Content
}
