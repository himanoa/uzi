{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Discord.Request.Internal (
  Request(..)
) where

import Data.Discord.Request.IdentifyRequest (IdentifyRequest)
import Data.Aeson
import GHC.Generics (Generic)

data Request = Identify IdentifyRequest
  deriving (Show, Eq)

instance ToJSON Request where
  toJSON x = case x of
    Identify ir -> object ["op" .= operationCode x, "d" .= toJSON ir]

operationCode :: Request -> Int
operationCode r = case r of
  Identify _ -> 2



