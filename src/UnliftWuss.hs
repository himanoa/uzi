module UnliftWuss (
  runClient,
  withPingThread
) where

import UnliftIO
import Network.Socket
import qualified Network.WebSockets as WS
import qualified Wuss as WS
import Network.WebSockets (Connection)

runClient :: (MonadUnliftIO m) => String -> PortNumber -> String -> WS.ConnectionOptions -> WS.Headers -> (WS.Connection -> m a) -> m a
runClient host port path opt headers inner =
  withRunInIO $ \runInIO ->
    WS.runSecureClientWith host port path opt headers (runInIO . inner)

withPingThread :: (MonadUnliftIO m) => Connection -> Int -> IO () -> m a -> m a
withPingThread conn interval after_sending inner =
  withRunInIO $ \runInIO -> 
    WS.withPingThread conn interval after_sending (runInIO  inner)
