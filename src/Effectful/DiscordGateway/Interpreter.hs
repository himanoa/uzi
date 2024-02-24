{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
module Effectful.DiscordGateway.Interpreter where

import Network.Socket
import Network.WebSockets (Connection)
import Network.WebSockets qualified as WS
import UnliftIO
import Wuss qualified as WS
import Effectful.DiscordGateway.Effect
import Effectful
import Data.Discord
import Data.Aeson (decode)
import Data.ByteString.Lazy (LazyByteString)
import Effectful.Dispatch.Dynamic (interpret)

import Data.ByteString.Lazy qualified as BL
import Data.Text.Encoding qualified as LT
import qualified Network.WebSockets as Wuss

runClient :: (MonadUnliftIO m) => String -> PortNumber -> String -> WS.ConnectionOptions -> WS.Headers -> (WS.Connection -> m a) -> m a
runClient host port path opt headers inner =
  withRunInIO $ \runInIO ->
    WS.runSecureClientWith host port path opt headers (runInIO . inner)

withPingThread :: (MonadUnliftIO m) => Connection -> Int -> IO () -> m a -> m a
withPingThread conn interval after_sending inner =
  withRunInIO $ \runInIO ->
    WS.withPingThread conn interval after_sending (runInIO inner)

handleEvent :: LazyByteString -> Maybe Response
handleEvent = decode @Response

withDiscordGatewayConnection  :: (MonadUnliftIO m) => (WS.Connection -> m a) -> m a
withDiscordGatewayConnection = runClient "gateway.discord.gg" 443 "/?v=10&encoding-json" WS.defaultConnectionOptions []

runDiscordGateway :: IOE :> es => WS.Connection -> Eff (DiscordGateway : es) a -> Eff es a
runDiscordGateway conn = interpret $ \_-> \case
  ReceiveEvent -> do
    d <- liftIO (Wuss.receiveData conn)
    pure . handleEvent $ (BL.fromStrict . LT.encodeUtf8 $ d)
  SendEvent t -> liftIO (Wuss.sendTextData conn t)
