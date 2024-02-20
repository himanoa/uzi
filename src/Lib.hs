{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Lib
  ( runUzi,
    UziError (CrashError),
    module Opcode,
  )
where

import Control.Exception.Safe
import Data.ByteString.Char8 qualified as ByteString
import Data.Text
import Data.Text.Encoding (decodeUtf8)
import Effectful (Eff, MonadUnliftIO (withRunInIO), liftIO, runEff, (:>))
import Effectful.Environment (Environment, lookupEnv, runEnvironment)
import Effectful.Log
import Log.Backend.StandardOutput
import Network.WebSockets (Connection)
import Network.WebSockets qualified as WS
import Opcode

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

runClient :: (MonadUnliftIO m) => String -> Int -> String -> WS.ConnectionOptions -> WS.Headers -> (Connection -> m a) -> m a
runClient host port path opt headers inner =
  withRunInIO $ \runInIO ->
    WS.runClientWith host port path opt headers (runInIO . inner)

runUzi :: IO ()

data UziError = CrashError
  deriving (Show)

instance Exception UziError

runUzi = runEff $ do
  withStdOutLogger $ \stdoutLogger -> do
    runEnvironment $ do
      runLog "Uzi" stdoutLogger defaultLogLevel $ do
        _ <- logInfo_ "Hello uzi"

        _ <- logInfo_ "Load enviroments"
        config <- fromEnvironment
        _ <- logInfo_ . pack . show $ config

        _ <-
          runClient
            "gateway.discord.gg"
            443
            ""
            WS.defaultConnectionOptions
            [("xxx", "yyy")]
            ( \c -> do
                _ <- liftIO (WS.sendTextData c (ByteString.pack "xxxx"))
                logInfo_ "Connected websocket"
            )
            `catch` ( \(e :: WS.HandshakeException) -> do
                        _ <- logAttention_ $ pack ("handshale failed " <> displayException e)
                        throw CrashError
                    )
        pure ()
