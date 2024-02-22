{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Data.Discord.SendOperation (
SendOperation(..),
operationCode
)where

import GHC.Generics
import Data.Aeson (ToJSON, FromJSON)
import Control.Lens.TH

data SendOperation
  = Identify 
  | Resume
  | Heartbeat
  | RequestGuildMembers
  | UpdateVoiceState
  | UpdatePresence
  deriving (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | get operationCode 
-- >>> operationCode Identify
-- 2
-- >>> operationCode Resume
-- 6
-- >>> operationCode Heartbeat
-- 1
-- >>> operationCode RequestGuildMembers
-- 8
-- >>> operationCode UpdateVoiceState
-- 4
-- >>> operationCode UpdatePresence
-- 3
--
operationCode :: SendOperation-> Int
operationCode code = case code of 
  Identify -> 2
  Resume -> 6
  Heartbeat -> 1
  RequestGuildMembers -> 8
  UpdateVoiceState -> 4
  UpdatePresence -> 3

makeLenses ''SendOperation
