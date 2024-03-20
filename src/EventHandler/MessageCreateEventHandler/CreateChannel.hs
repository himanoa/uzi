{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module EventHandler.MessageCreateEventHandler.CreateChannel where

import Control.Lens
import Data.Discord
import Data.Discord.Content
import Data.Discord.Response.MessageCreateEventResponse qualified as MCE
import Data.Uzi.OrganizeTimes
import Effectful
import Effectful.DiscordChannel
import Effectful.DynamicLogger
import Effectful.Error.Dynamic
import Effectful.NonDet
import Text.Parsec qualified as P
import Text.Parsec.Text qualified as P
import Data.Text

craeteChannelCommandParser :: P.Parser Text
craeteChannelCommandParser = do
  _ <- P.string "create-times"
  name <- P.many1 P.anyChar
  pure . pack $ name

createChannelEventHandler :: (DiscordChannel :> es, NonDet :> es, DynamicLogger :> es) => Response -> Eff es ()
createChannelEventHandler = \case
  MessageCreate res -> do
    case body (res ^. MCE.content) of
      Just command -> do
        let parseResultEither = P.runParser craeteChannelCommandParser () "CreateChannelCommand" command
        case parseResultEither of
          Left _ -> emptyEff
          Right name -> do
            let guildId = res ^. MCE.guildId
            info "CreateChannelEventHandler dispatched"
            createChannel guildId (makeCreateChannelParams . ChannelName $ "times-" <> name)
            sendMessage (makeMessage (res ^. MCE.channelId) (makeUnsafeContent ("timesを作ったよ -> #times-" <> name)))
            _ <-
              (runError @OrganizeTimesError . organizeTimes $ guildId) >>= \case
                Right _ -> sendMessage (makeMessage (res ^. MCE.channelId) (makeUnsafeContent "times channelをソートしたよ"))
                Left _ -> pure ()
            pure ()
      Nothing -> emptyEff
  _ -> emptyEff

