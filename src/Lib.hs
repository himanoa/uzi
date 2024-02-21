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
import Effectful (Eff, IOE, MonadUnliftIO (withRunInIO), liftIO, runEff, (:>))
import Effectful.Environment (Environment, lookupEnv, runEnvironment)
import Effectful.Log
import Log.Backend.StandardOutput
import Network.WebSockets (Connection)
import Network.WebSockets qualified as WS
import Opcode
import UnliftWuss qualified

data EnvConfig = EnvConfig
  {discordApiToken :: Text}
  deriving (Show)

makeEnvConfig :: Text -> EnvConfig
makeEnvConfig discordApiToken = EnvConfig {discordApiToken}

fromEnvironment :: (Environment :> es) => Eff es EnvConfig

data FromEnvironmentError = DiscordApiTokenIsUndefined
  deriving (Show)

instance Exception FromEnvironmentError

convertToText :: String -> Text
convertToText = decodeUtf8 . ByteString.pack

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
    runEnvironment $ do
      runLog "Uzi" stdoutLogger defaultLogLevel startUp

startUp :: (Log :> es, Environment :> es, IOE :> es) => Eff es ()
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

onConnect :: (Log :> es, IOE :> es) => Connection -> Eff es ()
onConnect c = do
  _ <- logInfo_ "Connected websocket"
  _ <- UnliftWuss.withPingThread c 15 (pure ()) $ do
    forever (receive c)
  pure ()

receive :: (Log :> es, IOE :> es) => Connection -> Eff es ()
receive conn = do
  d <- liftIO (WS.receiveData conn)
  _ <- logInfo_ d
  pure ()
