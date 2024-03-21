{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Data.Discord.Response.HelloEventResponse
  ( HelloEventResponse (..),
    interval
  )
where

import Data.Aeson 
import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.Discord.ReceiveEventOperationCode (ReceiveEventOperationCode (Hello))
import GHC.Generics
import Control.Lens

newtype HelloEventResponse = HelloEventResponse { _interval :: Int }
  deriving (Show, Generic, Eq)

makeLenses ''HelloEventResponse

-- | FromJson implementation
instance FromJSON HelloEventResponse where
  parseJSON = withObject "HelloEventResponse" $ \v -> do
    operationCode <- parseJSON @ReceiveEventOperationCode =<< v .: "op"
    case operationCode of
      Hello -> do
        dataSection <- v .: "d"
        i <- parseJSON @Int =<< dataSection .: "heartbeat_interval"
        pure $ HelloEventResponse { _interval = i}
      _ -> prependFailure "Not supported op code" (typeMismatch "op" "")
