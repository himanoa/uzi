{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Data.Discord.Internal where

import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics
import Control.Lens.TH

data ReceiveOperation
  = Hello
  | Ready
  | Resumed
  | Reconnected
  | InvalidSession
  | ApplicationCommandPermissionsUpdate
  | AutoModerationRuleCreate
  | AutoModerationRuleUpdate
  | AutoModerationRuleDelete
  | AutoModerationActionExecution
  | ChannelCreate
  | ChannelUpdate
  | ChannelDelete
  | ChannelPinsUpdate
  | ThreadCreate
  | ThreadUpdate
  | ThreadDelete
  | ThreadListSync
  | ThreadMemberUpdate
  | ThreadMembersUpdate
  | EntitlementCreate
  | EntitlementUpdate
  | EntitlementDelete
  | GuildCreate
  | GuildUpdate
  | GuildDelete
  | GuildAuditLogEntryCreate
  | GuildBanAdd
  | GuildBanRemove
  | GuildEmojisUpdate
  | GuildStickersUpdate
  | GuildIntegrationsUpdate
  | GuildMemberAdd
  | GuildMemberRemove
  | GuildMemberUpdate
  | GuildRoleCreate
  | GuildRoleUpdate
  | GuildRoleDelete
  | GuildScheduledEventCreate
  | GuildScheduledEventUpdate
  | GuildScheduledEventDelete
  | GuildScheduledEventUserAdd
  | GuildScheduledEventUserRemove
  | IntegrationCreate
  | IntegrationUpdate
  | IntegrationDelete
  | InteractionCreate
  | InviteCreate
  | InviteDelete
  | MessageCreate
  | MessageUpdate
  | MessageDelete
  | MessageDeleteBulk
  | MessageReactionAdd
  | MessageReactionRemove
  | MessageReactionRemoveAll
  | MessageReactionRemoveEmoji
  | PresenceUpdate
  | StageInstanceCreate
  | StageInstanceUpdate
  | StageInstanceDelete
  | TypingStart
  | UserUpdate
  | VoiceStateUpdate
  | VoiceServerUpdate
  | WebhooksUpdate
  deriving (Show, Enum, Generic)
  deriving anyclass (ToJSON, FromJSON)

data PayloadStructure d =  PayloadStructure {
  op :: Int,
  d :: d,
  t :: String
}
  deriving (Generic)

makeLenses ''PayloadStructure

deriving anyclass instance ToJSON a => ToJSON (PayloadStructure a)
deriving anyclass instance FromJSON a => FromJSON (PayloadStructure a)
deriving instance Show a => Show (PayloadStructure a)

data SendEventPayloadStructure d  =  SendEventPayloadStructure {
  op :: Int,
  d :: d,
  t :: String
}
  deriving (Generic)

makeLenses ''SendEventPayloadStructure

data ReceiveEventPayloadStructure d =  ReceiveEventPayloadStructure {
  op :: Int,
  d :: d,
  t :: String
}
  deriving (Generic)

makeLenses ''ReceiveEventPayloadStructure

