module Data.Uzi.HeartbeatInterval
  ( HeartbeatInterval (..),
    coerceHeartbeatInterval,
    makeHeartbeatInterval,
  )
where

import Data.Coerce (coerce)
import RIO

newtype HeartbeatInterval = HeartbeatInterval Int
  deriving (Show, Eq)

coerceHeartbeatInterval :: HeartbeatInterval -> Int
coerceHeartbeatInterval = coerce

makeHeartbeatInterval :: Int -> HeartbeatInterval
makeHeartbeatInterval = HeartbeatInterval
