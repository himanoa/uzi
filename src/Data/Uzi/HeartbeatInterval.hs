module Data.Uzi.HeartbeatInterval
  ( HeartbeatInterval (..),
    coerceHeartbeatInterval,
    makeHeartbeatInterval,
  )
where

import RIO
import Data.Coerce (coerce)

newtype HeartbeatInterval = HeartbeatInterval Int
  deriving (Show, Eq)

coerceHeartbeatInterval :: HeartbeatInterval -> Int
coerceHeartbeatInterval = coerce

makeHeartbeatInterval :: Int -> HeartbeatInterval
makeHeartbeatInterval = HeartbeatInterval
