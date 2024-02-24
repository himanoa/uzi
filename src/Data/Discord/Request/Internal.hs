{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
module Data.Discord.Request.Internal (
  Request(..)
) where

import Data.Discord.Request.IdentifyRequest (IdentifyRequest)
import Data.Aeson
import GHC.Generics (Generic)

data Request = Identify IdentifyRequest
  deriving (Generic, Show, Eq)
  deriving anyclass (ToJSON)
