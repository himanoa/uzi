{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Lib
  ( runUzi,
    UziError (CrashError),
  )
where

import Control.Exception.Safe
import Control.Monad (forever)
import Data.ByteString.Char8 qualified as ByteString
import Data.Discord
import Data.Text
import Data.Text.Encoding (decodeUtf8)
import Effectful (Eff, IOE, runEff, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.Async (concurrently_, runConcurrent)
import Effectful.Concurrent.STM (TQueue, atomically, newTQueue, readTQueue, writeTQueue)
import Effectful.DiscordGateway
import Effectful.DynamicLogger
import Effectful.Environment (Environment, lookupEnv, runEnvironment)
import Effectful.Log
import EnvConfig
import EventHandler.HelloEventHandler (helloEventHandler)
import Log.Backend.StandardOutput
import Network.WebSockets (Connection)

makeEnvConfig :: Text -> EnvConfig
makeEnvConfig discordApiToken = EnvConfig {discordApiToken}

data FromEnvironmentError = DiscordApiTokenIsUndefined
  deriving (Show)

instance Exception FromEnvironmentError

convertToText :: String -> Text
convertToText = decodeUtf8 . ByteString.pack

fromEnvironment :: (Environment :> es) => Eff es EnvConfig
fromEnvironment = do
  discordApiTokenMaybe <- lookupEnv "UZI_DISCORD_API_TOKEN"
  discordApiToken <- maybe (throw DiscordApiTokenIsUndefined) pure discordApiTokenMaybe
  pure $ makeEnvConfig . convertToText $ discordApiToken

runUzi :: IO ()

data UziError = CrashError
  deriving (Show)

instance Exception UziError

runUzi = runEff $ do
  withStdOutLogger $ \stdoutLogger -> do
    runConcurrent $ do
      runEnvironment $ do
        runLog "Uzi" stdoutLogger defaultLogLevel $ do
          runDynamicLogger startUp

startUp :: (DynamicLogger :> es, IOE :> es, Concurrent :> es, Environment :> es) => Eff es ()
startUp = do
  _ <- info "Hello uzi"
  _ <- info "Load enviroments"
  withDiscordGatewayConnection onConnect
  pure ()

onConnect :: (DynamicLogger :> es, IOE :> es, Concurrent :> es, Environment :> es) => Connection -> Eff es ()
onConnect c = do
  _ <- info "Connected websocket"
  _ <- withPingThread c 15 (pure ()) $ do
    queue <- atomically newTQueue
    concurrently_ (forever (runDiscordGateway c (receiver queue))) (forever $ runDiscordGateway c (sender queue))
  -- TODO: Handle Ctrl + C and kill signal
  pure ()

receiver :: (Concurrent :> es, DiscordGateway :> es) => TQueue Response -> Eff es ()
receiver queue = do
  _ <-
    receiveEvent >>= \case
      Just x -> atomically $ writeTQueue queue x
      Nothing -> pure ()
  pure ()

data SenderError = EventQueueIsEmpty
  deriving (Show)

sender :: (DynamicLogger :> es, Concurrent :> es, DiscordGateway :> es, Environment :> es) => TQueue Response -> Eff es ()
sender queue = do
  event <- atomically $ readTQueue queue
  config <- fromEnvironment
  info . convertToText $ ("Received Log " <> show event)
  helloEventHandler config event
