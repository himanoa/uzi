{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Lib
  ( runUzi,
    UziError (CrashError),
    module Opcode,
  )
where

import Control.Exception.Safe
import Control.Monad (forever)
import Data.ByteString.Char8 qualified as ByteString
import Data.Text
import Data.Text.Encoding (decodeUtf8)
import Effectful (Eff, IOE, runEff, (:>))
import Effectful.Concurrent.Async ( concurrently_, runConcurrent )
import Effectful.Environment (Environment, lookupEnv, runEnvironment)
import Effectful.Log
import Log.Backend.StandardOutput
import Network.WebSockets (Connection)
import Opcode
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.STM (TQueue, atomically, writeTQueue, newTQueue, readTQueue)
import Effectful.DiscordGateway
import Data.Discord

data EnvConfig = EnvConfig
  {discordApiToken :: Text}
  deriving (Show)

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
        runLog "Uzi" stdoutLogger defaultLogLevel startUp

startUp :: (Log :> es, Environment :> es, IOE :> es, Concurrent :> es) => Eff es ()
startUp = do
  _ <- logInfo_ "Hello uzi"
  _ <- logInfo_ "Load enviroments"
  config <- fromEnvironment
  _ <- logInfo_ . pack . show $ config
  withDiscordGatewayConnection onConnect
  pure ()

onConnect :: (Log :> es, IOE :> es, Concurrent :> es) => Connection -> Eff es ()
onConnect c = do
  _ <- logInfo_ "Connected websocket"
  _ <- withPingThread c 15 (pure ()) $ do
    queue <- atomically newTQueue
    concurrently_ (forever (runDiscordGateway c (receiver queue))) (forever $ runDiscordGateway c (sender queue))
  -- TODO: Handle Ctrl + C and kill signal
  pure ()

receiver :: (Log :> es, IOE :> es, Concurrent :> es, DiscordGateway :> es) => TQueue Response -> Eff es ()
receiver queue = do
  event <- receiveEvent
  _ <- case event of
    Just x ->  atomically $ writeTQueue queue x
    Nothing -> pure ()
  pure ()


data SenderError = EventQueueIsEmpty
  deriving (Show)

sender :: (Log :> es, IOE :> es, Concurrent :> es, DiscordGateway :> es) => TQueue Response ->  Eff es ()
sender queue = do
  event <- atomically $ readTQueue queue
  logInfo_ . convertToText $ ("Received Log " <> show event)
  -- case eventMaybe of
  --   Just event ->  logInfo_ . convertToText $ ("Received Log" <> show event)
  --   Nothing -> pure ()
