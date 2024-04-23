{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Data.Discord.Mention where

import Control.Lens
import Data.Aeson
import Data.Discord.GlobalName
import Data.Discord.UserId
import Data.Discord.UserName
import RIO

data Mention = Mention
  { _id :: UserId,
    _username :: UserName,
    _globalname :: Maybe GlobalName,
    _bot :: Bool
  }
  deriving (Show, Eq)

makeLenses ''Mention

instance FromJSON Mention where
  parseJSON = withObject "Mention" $ \o -> do
    _id <- parseJSON @UserId =<< o .: "id"
    _username <- parseJSON @UserName =<< o .: "username"
    _globalname <- o .:? "globalname"
    _bot <- fromMaybe False <$> o .:? "bot"
    pure Mention {..}

makeMention :: UserId -> UserName -> Maybe GlobalName -> Bool -> Mention
makeMention _id _username _globalname _bot = Mention {..}
