{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Discord.Request.Internal
  ( Request (..),
  )
where

import RIO
import Data.Aeson
import Data.Discord.Request.IdentifyRequest (IdentifyRequest)

data Request where
  Identify :: IdentifyRequest -> Request
  Heartbeat :: Request
  deriving (Show, Eq)

instance ToJSON Request where
  toJSON x = case x of
    Identify ir -> object ["op" .= operationCode x, "d" .= toJSON ir]
    Heartbeat -> object ["op" .= operationCode x, "d" .= (251 :: Int)]

operationCode :: Request -> Int
operationCode r = case r of
  Identify _ -> 2
  Heartbeat -> 1
