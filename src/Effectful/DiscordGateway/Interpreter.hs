{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Effectful.DiscordGateway.Interpreter where

import Data.Aeson (eitherDecode, encode)
import Data.ByteString.Lazy qualified as LB
import Data.Discord
import Data.String.Conversions (ConvertibleStrings (convertString))
import Data.Text
import Data.Text.Encoding (encodeUtf8)
import Effectful
import Effectful.DiscordGateway.Effect
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.DynamicLogger
import Effectful.Environment
import Network.Socket
import Network.WebSockets (Connection)
import Network.WebSockets qualified as WS
import Network.WebSockets qualified as Wuss
import RIO qualified
import Wuss qualified as WS

runClient :: (MonadUnliftIO m) => String -> PortNumber -> String -> WS.ConnectionOptions -> WS.Headers -> (WS.Connection -> m a) -> m a
runClient host port path opt headers inner =
  withRunInIO $ \runInIO ->
    WS.runSecureClientWith host port path opt headers (runInIO . inner)

withPingThread :: (MonadUnliftIO m) => Connection -> Int -> IO () -> m a -> m a
withPingThread conn interval after_sending inner =
  withRunInIO $ \runInIO ->
    WS.withPingThread conn interval after_sending (runInIO inner)

handleEvent :: LB.LazyByteString -> Either String Response
handleEvent = eitherDecode @Response

withDiscordGatewayConnection :: (MonadUnliftIO m) => (WS.Connection -> m a) -> m a
withDiscordGatewayConnection = runClient "gateway.discord.gg" 443 "/?v=14&encoding-json" WS.defaultConnectionOptions []

runDiscordGateway :: (IOE :> es, Environment :> es, DynamicLogger :> es) => WS.Connection -> Eff (DiscordGateway : es) a -> Eff es a
runDiscordGateway conn = interpret $ \_ -> \case
  ReceiveEvent -> do
    d <- liftIO . Wuss.receiveData @LB.LazyByteString $ conn
    lookupEnv "UZI_IS_DEBUG" >>= \case
      Just _ -> info . RIO.displayBytesUtf8 . encodeUtf8 . convertString $ d
      Nothing -> pure ()
    case handleEvent d of
      Left s -> do
        attention . RIO.displayBytesUtf8 . encodeUtf8 . pack $ s
        pure Nothing
      Right p -> do
        pure . Just $ p
  SendEvent request -> do
    let text = encode request
    lookupEnv "UZI_IS_DEBUG" >>= \case
      Just _ -> info . RIO.displayShow $ text
      Nothing -> pure ()
    liftIO . Wuss.sendTextData conn $ text
