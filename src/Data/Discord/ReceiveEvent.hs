{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Data.Discord.ReceiveEvent where

import RIO
import Control.Lens (makeLenses)
import Data.Aeson

data ReceiveEvent
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

makeLenses ''ReceiveEvent
