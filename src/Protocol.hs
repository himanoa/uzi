module Protocol (
  Protocol(..) 
) where

data Protocol
  = ReceiveHelloEvent 
  | ReadyEvent
  | GatewayDisconnectedOccurus
  deriving (Show, Eq)

