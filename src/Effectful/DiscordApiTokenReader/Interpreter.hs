{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Effectful.DiscordApiTokenReader.Interpreter where

import RIO
import Control.Exception
import Data.String.Conversions (ConvertibleStrings (convertString))
import Effectful
import Effectful.DiscordApiTokenReader.Effect
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Environment

data FromEnvironmentError = DiscordApiTokenIsUndefined
  deriving (Show)

instance Exception FromEnvironmentError

runDiscordApiTokenReader :: (Environment :> es) => Eff (DiscordApiTokenReader : es) a -> Eff es a
runDiscordApiTokenReader = interpret $ \_ -> \case
  GetToken ->
    lookupEnv "UZI_DISCORD_API_TOKEN" >>= \case
      Just token -> pure . convertString $ token
      Nothing -> throw DiscordApiTokenIsUndefined
