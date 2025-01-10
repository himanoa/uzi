{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Lib
  ( runUzi,
    UziError (CrashError),
  )
where

import Control.Exception.Safe
import Data.Discord
import Data.Discord.Request.SlashCommand
import Data.Discord.User
import Data.Uzi.HeartbeatInterval
import Effectful (Eff, IOE, runEff, (:>))
import Effectful.BotUser
import Effectful.Concurrent (Concurrent, threadDelay)
import Effectful.Concurrent.Async (forConcurrently_, runConcurrent)
import Effectful.Concurrent.STM (TQueue, atomically, newTQueue, readTQueue, writeTQueue)
import Effectful.DiscordApiTokenReader
import Effectful.DiscordApplication (runDiscordApplication)
import Effectful.DiscordChannel
import Effectful.DiscordGateway
import Effectful.DiscordSlash
import Effectful.DiscordSlash.Effect
import Effectful.Dispatch.Dynamic
import Effectful.DynamicLogger
import Effectful.Environment (Environment, runEnvironment)
import Effectful.InteractionCallback.Effect
import Effectful.InteractionCallback.Interpreter (runInteractionCallback)
import Effectful.Log.Static
import Effectful.Req
import Effectful.State.Static.Shared
import EventHandler
import Network.WebSockets (Connection)
import RIO hiding (TQueue, atomically, catch, forConcurrently_, newTQueue, readTQueue, threadDelay, writeTQueue)

data FromEnvironmentError = DiscordApiTokenIsUndefined
  deriving (Show)

instance Exception FromEnvironmentError

runUzi :: IO ()

data UziError = CrashError
  deriving (Show)

instance Exception UziError

runUzi = runEff $ do
  logOptions <- logOptionsHandle RIO.stderr True
  runConcurrent
    . runEnvironment
    . runLog logOptions
    . runDynamicLogger
    . runDiscordApiTokenReader
    . runRequest
    . runDiscordApplication
    . runDiscordChannel
    . runRegisterSlash
    . runInteractionCallback
    . evalState @(Maybe User) Nothing
    . runBotUser
    . evalState @(Maybe HeartbeatInterval) Nothing
    $ startUp

startUp ::
  ( DynamicLogger :> es,
    IOE :> es,
    Concurrent :> es,
    Environment :> es,
    DiscordApiTokenReader :> es,
    DiscordChannel :> es,
    BotUser :> es,
    SlashCommand :> es,
    State (Maybe HeartbeatInterval) :> es,
    InteractionCallback :> es
  ) =>
  Eff es ()
startUp = do
  _ <- info "Hello uzi"

  _ <- info "Register Slash"

  _ <-
    send
      $ makeGlobalSlashCommand
        "create-times"
        "#times-<あなたの名前> のtimesチャンネルが作成され、timesチャンネルがソートされます"
        [StringOption (Name "name") (Description "times-{name} という名前で作成します。") True]
  threadDelay 5000000
  _ <-
    send
      $ makeGlobalSlashCommand
        "organize-times"
        "timesチャンネルを times- prefixを除いた A-M N-Z のグループにグルーピングしてから、timesチャンネルの並び順をソートします"
        []
  threadDelay 5000000
  _ <-
    send
      $ makeGlobalSlashCommand
        "help"
        "helpの書かれたリンクが投稿されます"
        []

  _ <- info "Load enviroments"
  withDiscordGatewayConnection onConnect
  pure ()

onConnect ::
  ( DynamicLogger :> es,
    IOE :> es,
    Concurrent :> es,
    Environment :> es,
    DiscordApiTokenReader :> es,
    DiscordChannel :> es,
    BotUser :> es,
    State (Maybe HeartbeatInterval) :> es,
    InteractionCallback :> es
  ) =>
  Connection ->
  Eff es ()
onConnect c = do
  _ <- info "Connected websocket"
  _ <- withPingThread c 15 (pure ()) $ do
    queue <- atomically newTQueue
    forConcurrently_
      [ receiver queue `catch` \EventNotFound -> pure (),
        sender queue,
        sendHeartbeat `catch` \MissingHeartbeatInterval -> pure ()
      ]
      (forever . runDiscordGateway c)
  -- TODO: Handle Ctrl + C and kill signal
  pure ()

data ReceiverError = EventNotFound
  deriving (Show, Eq, Exception)

receiver :: (Concurrent :> es, DiscordGateway :> es) => TQueue Response -> Eff es ()
receiver queue = do
  event <- RIO.maybe (RIO.throwM EventNotFound) pure =<< receiveEvent
  atomically $ writeTQueue queue event

data SenderError = EventQueueIsEmpty
  deriving (Show)

sender ::
  ( DynamicLogger :> es,
    Concurrent :> es,
    DiscordGateway :> es,
    DiscordApiTokenReader :> es,
    DiscordChannel :> es,
    BotUser :> es,
    State (Maybe HeartbeatInterval) :> es,
    InteractionCallback :> es
  ) =>
  TQueue Response ->
  Eff es ()
sender queue = do
  event <- atomically $ readTQueue queue
  dispatchEventHandlers event

data SendHeartbeatError = MissingHeartbeatInterval
  deriving (Show, Eq, Exception)

sendHeartbeat ::
  ( DiscordGateway :> es,
    Concurrent :> es,
    State (Maybe HeartbeatInterval) :> es,
    DynamicLogger :> es
  ) =>
  Eff es ()
sendHeartbeat = do
  intervalMaybe <- get @(Maybe HeartbeatInterval)
  interval <- maybe (RIO.throwM MissingHeartbeatInterval) pure intervalMaybe
  info "Start send heartbeart"
  sendEvent Heartbeat
  info "Sent heartbeart"
  RIO.void . threadDelay $ (coerceHeartbeatInterval interval * 1000 * (1 :: Int))
