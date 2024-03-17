{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module EventHandler.MessageCreateEventHandler
  ( dispatchMessageEventHandlers,
  )
where

import Control.Lens
import Data.Discord
import Data.Discord.Mention qualified as M
import Data.Discord.User qualified as U
import Data.Discord.Response.MessageCreateEventResponse (isBot, mentions)
import Effectful
import Effectful.DiscordChannel
import Effectful.NonDet
import EventHandler.MessageCreateEventHandler.Ping
import Effectful.BotUser

dispatchMessageEventHandlers :: (DiscordChannel :> es, NonDet :> es, BotUser :> es) => Response -> Eff es ()
dispatchMessageEventHandlers res = case res of
  MessageCreate e -> getBotUser >>= \case
    Just botUser -> do
      let mentionIds = map (^. M.id) (e ^. mentions)
      if not (e ^. isBot) && any (\m -> m == (botUser ^. U.id)) mentionIds
        then pingEventHandler res
        else emptyEff
    Nothing -> emptyEff
  _ -> emptyEff
