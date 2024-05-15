{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Data.Discord.Request.SlashCommand
  ( Name (..),
    Description (..),
    CommandOption (..),
  )
where

import Data.Aeson
import RIO.Text qualified as DT
import RIO

newtype Name = Name DT.Text
  deriving (Eq)
  deriving (ToJSON, Show) via DT.Text

newtype Description = Description DT.Text
  deriving (Eq)
  deriving (ToJSON, Show) via DT.Text

data CommandOption = StringOption !Name !Description !Bool | ChannelOption !Name !Description !Bool
  deriving (Eq)
  deriving (Show)

instance ToJSON CommandOption where
  toJSON :: CommandOption -> Value
  toJSON (StringOption name description required) =
    object
      [ "name" .= name,
        "description" .= description,
        "required" .= required,
        "type" .= (3 :: Integer)
      ]
  toJSON (ChannelOption name description required) =
    object
      [ "name" .= name,
        "description" .= description,
        "required" .= required,
        "type" .= (7 :: Integer)
      ]
