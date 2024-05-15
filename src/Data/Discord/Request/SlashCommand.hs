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
import RIO
import RIO.Text qualified as T

newtype Name = Name T.Text
  deriving (Eq)
  deriving (ToJSON, Show) via T.Text

newtype Description = Description T.Text
  deriving (Eq)
  deriving (ToJSON, Show) via T.Text

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
