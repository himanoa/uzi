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
import Effectful.DiscordApiTokenReader
import Effectful.DiscordChannel
import Effectful.DiscordGateway
import Effectful.DynamicLogger
import Effectful.Environment (Environment, runEnvironment)
import Effectful.Log
import Effectful.Req
import EventHandler
import Log.Backend.StandardOutput
import Network.WebSockets (Connection)
import Effectful.BotUser
import Data.Discord.User 
import Effectful.State.Static.Shared 

data FromEnvironmentError = DiscordApiTokenIsUndefined
  deriving (Show)

instance Exception FromEnvironmentError

convertToText :: String -> Text
convertToText = decodeUtf8 . ByteString.pack

runUzi :: IO ()

data UziError = CrashError
  deriving (Show)

instance Exception UziError

runUzi = runEff $ withStdOutLogger $ \stdoutLogger -> runConcurrent . runEnvironment . runLog "Uzi" stdoutLogger defaultLogLevel . runDynamicLogger . runDiscordApiTokenReader . runRequest . runDiscordChannel . evalState @(Maybe User) Nothing . runBotUser $ startUp

startUp :: (DynamicLogger :> es, IOE :> es, Concurrent :> es, Environment :> es, DiscordApiTokenReader :> es, DiscordChannel :> es, BotUser :> es) => Eff es ()
startUp = do
  _ <- info "Hello uzi"
  _ <- info "Load enviroments"
  withDiscordGatewayConnection onConnect
  pure ()

onConnect :: (DynamicLogger :> es, IOE :> es, Concurrent :> es, Environment :> es, DiscordApiTokenReader :> es, DiscordChannel :> es, BotUser :> es) => Connection -> Eff es ()
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

sender :: (DynamicLogger :> es, Concurrent :> es, DiscordGateway :> es, DiscordApiTokenReader :> es, DiscordChannel :> es, BotUser :> es) => TQueue Response -> Eff es ()
sender queue = do
  event <- atomically $ readTQueue queue
  info . convertToText $ ("Received Log " <> show event)
  dispatchEventHandlers event
