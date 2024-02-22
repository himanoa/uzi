{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Discord.Internal where

import Control.Lens.TH
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics

data PayloadStructure d = PayloadStructure
  { op :: Int,
    d :: d,
    t :: String
  }
  deriving (Generic)

makeLenses ''PayloadStructure

deriving anyclass instance (ToJSON a) => ToJSON (PayloadStructure a)

deriving anyclass instance (FromJSON a) => FromJSON (PayloadStructure a)

deriving instance (Show a) => Show (PayloadStructure a)

data SendEventPayloadStructure d = SendEventPayloadStructure
  { op :: Int,
    d :: d,
    t :: String
  }
  deriving (Generic)

makeLenses ''SendEventPayloadStructure

data ReceiveEventPayloadStructure d = ReceiveEventPayloadStructure
  { op :: Int,
    d :: d,
    t :: String
  }
  deriving (Generic)

makeLenses ''ReceiveEventPayloadStructure
