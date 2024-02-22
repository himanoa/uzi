module UnliftWuss
  ( runClient,
    withPingThread,
  )
where

import Network.Socket
import Network.WebSockets (Connection)
import Network.WebSockets qualified as WS
import UnliftIO
import Wuss qualified as WS

runClient :: (MonadUnliftIO m) => String -> PortNumber -> String -> WS.ConnectionOptions -> WS.Headers -> (WS.Connection -> m a) -> m a
runClient host port path opt headers inner =
  withRunInIO $ \runInIO ->
    WS.runSecureClientWith host port path opt headers (runInIO . inner)

withPingThread :: (MonadUnliftIO m) => Connection -> Int -> IO () -> m a -> m a
withPingThread conn interval after_sending inner =
  withRunInIO $ \runInIO ->
    WS.withPingThread conn interval after_sending (runInIO inner)
