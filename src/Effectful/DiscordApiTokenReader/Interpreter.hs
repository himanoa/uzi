{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Effectful.DiscordApiTokenReader.Interpreter where

import Effectful
import Effectful.Environment
import Effectful.DiscordApiTokenReader.Effect
import Effectful.Dispatch.Dynamic (interpret)
import Control.Exception
import Data.String.Conversions (ConvertibleStrings(convertString))

data FromEnvironmentError = DiscordApiTokenIsUndefined
  deriving (Show)

instance Exception FromEnvironmentError

runDiscordApiTokenReader :: (Environment :> es) => Eff (DiscordApiTokenReader : es) a -> Eff es a
runDiscordApiTokenReader = interpret $ \_ -> \case
  GetToken -> do
    tokenMaybe <- lookupEnv "UZI_DISCORD_API_TOKEN"
    case tokenMaybe of
      Just token -> pure . convertString $ token
      Nothing -> throw DiscordApiTokenIsUndefined


