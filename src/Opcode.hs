module Opcode
  ( OperationCode (..),
  )
where

-- Deprecated
data OperationCode
  = Identify
  | SelectProtocol
  | Ready
  | Heartbeat
  | SessionDescription
  | Speaking
  | HeartbeatAck
  | Resume
  | Hello
  | Resumed
  | ClientConnect
  | ClientDisconnect
  deriving (Show, Enum)