{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Discord.Request.Internal
  ( Request (..),
  )
where

import Data.Aeson
import Data.Discord.Request.IdentifyRequest (IdentifyRequest)

data Request where
  Identify :: IdentifyRequest -> Request
  deriving (Show, Eq)

instance ToJSON Request where
  toJSON x = case x of
    Identify ir -> object ["op" .= operationCode x, "d" .= toJSON ir]

operationCode :: Request -> Int
operationCode r = case r of
  Identify _ -> 2
