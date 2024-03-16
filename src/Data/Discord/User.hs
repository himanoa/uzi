{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Data.Discord.User where

import Data.Discord.UserId
import Data.Default
import Data.Discord.UserName 
import Data.Discord.GlobalName (GlobalName (GlobalName))
import Data.Aeson
import Control.Lens
import GHC.Generics (Generic)

data User = User {
  _id :: UserId,
  _username :: UserName,
  _globalname :: Maybe GlobalName
}
  deriving (Show, Eq, Generic)

instance FromJSON User where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

instance ToJSON User where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance Default User where
  def = User { 
    _id = UserId "",
    _username = UserName "",
    _globalname = Just . GlobalName $ ""
  }


makeLenses ''User

