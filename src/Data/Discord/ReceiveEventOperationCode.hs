{-# LANGUAGE OverloadedStrings #-}

module Data.Discord.ReceiveEventOperationCode (

) where
import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.Aeson
import Data.Scientific

data ReceiveEventOperationCode = Hello deriving Show


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
--
instance FromJSON ReceiveEventOperationCode where
  parseJSON = withScientific "ReceiveEventOperationCode" $ \v  -> case toBoundedInteger @Int v of
    Just 10 -> pure Hello
    op -> prependFailure ("Not supported op code " <> show op) (typeMismatch "Opcode" (Number v))

