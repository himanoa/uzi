{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Discord.Response.HelloEventResponse (
  HelloEventResponse(..)
) where

import GHC.Generics
import Data.Discord.ReceiveEventOperationCode (ReceiveEventOperationCode (Hello))
import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import Data.Aeson.Types (prependFailure, typeMismatch)

data HelloEventResponse = HelloEventResponse 
  deriving (Show, Generic, Eq) 

--
-- | FromJson implementation
--
instance FromJSON HelloEventResponse where
  parseJSON = withObject "HelloEventResponse" $ \v -> do
    operationCode <- parseJSON @ReceiveEventOperationCode =<< v .: "op"
    case operationCode of
      Hello -> pure HelloEventResponse
      _ -> prependFailure "Not supported op code" ( typeMismatch "op" "" )
    
