{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module EventHandler.MessageCreateEventHandler
  ( dispatchMessageEventHandlers,
  )
where

import Control.Lens
import Data.Discord
import Data.Discord.Mention qualified as M
import Data.Discord.Response.MessageCreateEventResponse (isBot, mentions)
import Data.Discord.User qualified as U
import Effectful
import Effectful.BotUser
import Effectful.DiscordChannel
import Effectful.DynamicLogger
import Effectful.NonDet
import EventHandler.MessageCreateEventHandler.OrganizeTimes
import EventHandler.MessageCreateEventHandler.Ping

dispatchMessageEventHandlers :: (DiscordChannel :> es, NonDet :> es, BotUser :> es, DynamicLogger :> es) => Response -> Eff es ()
dispatchMessageEventHandlers res = case res of
  MessageCreate e ->
    getBotUser >>= \case
      Just botUser -> do
        let mentionIds = map (^. M.id) (e ^. mentions)
        if not (e ^. isBot) && any (\m -> m == (botUser ^. U.id)) mentionIds
          then pingEventHandler res <|> organizeTimesHandler res
          else emptyEff
      Nothing -> emptyEff
  _ -> emptyEff
