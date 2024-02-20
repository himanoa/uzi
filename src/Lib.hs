{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Lib
  ( runUzi,
    module Opcode,
  )
where

import Control.Exception (Exception, throw)
import Data.ByteString.Char8 qualified as ByteString
import Data.Text
import Data.Text.Encoding (decodeUtf8)
import Effectful (Eff, MonadUnliftIO (withRunInIO), liftIO, runEff, (:>))
import Effectful.Environment (Environment, lookupEnv, runEnvironment)
import Network.WebSockets (Connection)
import Network.WebSockets qualified as WS
import Opcode

data EnvConfig = EnvConfig
  { discordApiToken :: Text
  }
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
runUzi = runEff $ do
  -- withStdOutLogger $ \stdoutLogger -> do
  runEnvironment $ do
    -- runLog "Uzi" stdoutLogger defaultLogLevel $ do
    --        logInfo_ "Hello uzi"
    --        logInfo_ "Load enviroments"
    config <- fromEnvironment
    --       _ <- logInfo_ . pack . show $ config
    runClient "localhost" 1337 "" WS.defaultConnectionOptions [("xxx", "yyy")] $ \c -> do
      _ <- liftIO (WS.sendTextData c (ByteString.pack "xxxx"))
      pure ()

--          logInfo_ "Connected websocket"
