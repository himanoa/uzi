{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Data.Discord.Response.HelloEventResponse
  ( HelloEventResponse (..),
    HelloEventResponse,
  )
where

import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.Discord.ReceiveEventOperationCode (ReceiveEventOperationCode (Hello))
import GHC.Generics

data HelloEventResponse = HelloEventResponse
  deriving (Show, Generic, Eq)

-- | FromJson implementation
instance FromJSON HelloEventResponse where
  parseJSON = withObject "HelloEventResponse" $ \v -> do
    operationCode <- parseJSON @ReceiveEventOperationCode =<< v .: "op"
    case operationCode of
      Hello -> pure HelloEventResponse
      _ -> prependFailure "Not supported op code" (typeMismatch "op" "")
