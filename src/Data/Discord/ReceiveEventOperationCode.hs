{-# LANGUAGE OverloadedStrings #-}

module Data.Discord.ReceiveEventOperationCode
  ( ReceiveEventOperationCode (..),
  )
where

import Data.Aeson
import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.Scientific

data ReceiveEventOperationCode = Hello | Ready | HeartbeatAck deriving (Show)

-- | ReceiveEventOperationCode is decodable from Json
--
-- >>> {-# LANGUAGE OverloadedStrings #-}
-- >>> import Data.ByteString.Lazy
-- >>> let json = "10"
-- >>> decode @ReceiveEventOperationCode json
-- Just Hello
--
-- >>> {-# LANGUAGE OverloadedStrings #-}
-- >>> import Data.ByteString.Lazy
-- >>> let json = "'10'"
-- >>> decode @ReceiveEventOperationCode json
-- Nothing
instance FromJSON ReceiveEventOperationCode where
  parseJSON = withScientific "ReceiveEventOperationCode" $ \v -> case toBoundedInteger @Int v of
    Just 10 -> pure Hello
    Just 11 -> pure HeartbeatAck
    Just 0 -> pure Ready
    op -> prependFailure ("Not supported op code " <> show op) (typeMismatch "Opcode" (Number v))
