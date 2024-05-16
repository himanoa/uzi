{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- |
-- Module: Effectful.InteractionCallback.Effect
-- Description: UziBotのApplication Idを取得するEffect
-- Maintainer: himanoa <matsunoappy@gmail.com>
--
-- UziBotのApplication Idを読み出すためのEffect Moduleです
module Data.Discord.MessageFlags
  ( MessageFlags (..),
    flagEphemeral,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Bits (Bits (shiftL))
import RIO

newtype MessageFlags = MessageFlags Integer
  deriving (Show, Eq)
  deriving (Bits) via Integer
  deriving (ToJSON, FromJSON) via Integer

-- https://discord.com/developers/docs/resources/channel#message-object-message-flags
flagEphemeral :: MessageFlags
flagEphemeral = MessageFlags $ shiftL 1 6
