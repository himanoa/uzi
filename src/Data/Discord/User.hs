{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Data.Discord.User where

import RIO
import Control.Lens
import Data.Aeson
import Data.Default
import Data.Discord.GlobalName (GlobalName (GlobalName))
import Data.Discord.UserId
import Data.Discord.UserName

data User = User
  { _id :: UserId,
    _username :: UserName,
    _globalname :: Maybe GlobalName
  }
  deriving (Show, Eq, Generic)

instance FromJSON User where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}

instance ToJSON User where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 1}

instance Default User where
  def =
    User
      { _id = UserId "",
        _username = UserName "",
        _globalname = Just . GlobalName $ ""
      }

makeLenses ''User
