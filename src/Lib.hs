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
import Effectful (Eff, IOE, liftIO, runEff, (:>))
import Effectful.Concurrent.Async ( concurrently_, runConcurrent )
import Effectful.Environment (Environment, lookupEnv, runEnvironment)
import Effectful.Log
import Log.Backend.StandardOutput
import Network.WebSockets (Connection)
import Network.WebSockets qualified as WS
import Opcode
import UnliftWuss qualified
import Data.Discord.Response.HelloEventResponse
import Data.Aeson (eitherDecode)
import qualified Data.Text.Encoding as LT
import Data.ByteString.Lazy qualified as BL
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.STM (TQueue, atomically, writeTQueue, newTQueue, readTQueue)

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

  _ <-
    UnliftWuss.runClient
      "gateway.discord.gg"
      443
      "/?v=10&encoding-json"
      WS.defaultConnectionOptions
      []
      onConnect
      `catch` ( \(e :: WS.HandshakeException) -> do
                  _ <- logAttention_ $ pack ("handshake failed " <> displayException e)
                  throw CrashError
              )
  pure ()

onConnect :: (Log :> es, IOE :> es, Concurrent :> es) => Connection -> Eff es ()
onConnect c = do
  _ <- logInfo_ "Connected websocket"
  _ <- UnliftWuss.withPingThread c 15 (pure ()) $ do
    queue <- atomically newTQueue
    concurrently_ (forever $ receiver c queue) (forever $ sender c queue)
  -- TODO: Handle Ctrl + C and kill signal
  pure ()

receiver :: (Log :> es, IOE :> es, Concurrent :> es) => Connection -> TQueue HelloEventResponse -> Eff es ()
receiver conn queue = do
  d <- liftIO (WS.receiveData conn)
  _ <- case eitherDecode @HelloEventResponse (BL.fromStrict . LT.encodeUtf8 $ d) of
    Right x ->  atomically $ writeTQueue queue x
    Left _-> pure ()
  _ <- logInfo_ d
  pure ()


data SenderError = EventQueueIsEmpty
  deriving (Show)

sender :: (Log :> es, IOE :> es, Concurrent :> es) => Connection -> TQueue HelloEventResponse ->  Eff es ()
sender conn queue = do
  event <- atomically $ readTQueue queue
  logInfo_ . convertToText $ ("Received Log" <> show event)
  -- case eventMaybe of
  --   Just event ->  logInfo_ . convertToText $ ("Received Log" <> show event)
  --   Nothing -> pure ()
