{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module: EventHandler.MessageCreateEventHandler
-- Description: Discordが送信してくるMessageCreateのイベントのハンドラです
-- Maintainer: himanoa <matsunoappy@gmail.com>
--
-- Discordが送信してくるMessageCreateイベントのイベントハンドラです。
--
-- 主にサーバー上でメッセージが投稿された時に実行されます
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
import EventHandler.MessageCreateEventHandler.CreateChannel (createChannelEventHandler)
import EventHandler.MessageCreateEventHandler.Help
import EventHandler.MessageCreateEventHandler.OrganizeTimes
import EventHandler.MessageCreateEventHandler.Ping
import RIO hiding ((^.))

-- | メッセージが投稿された時に実行されるイベントハンドラです
--
-- 内部で実行したいコマンドごとにハンドラを分けるためにNonDet Effectに依存しています
-- また、BotUserの投稿に反応してしまうと、自分の発言に自分で発言してしまうためこの関数で止めています
dispatchMessageEventHandlers :: (DiscordChannel :> es, NonDet :> es, BotUser :> es, DynamicLogger :> es) => Response -> Eff es ()
dispatchMessageEventHandlers res = case res of
  MessageCreate e ->
    getBotUser >>= \case
      Just botUser -> do
        let mentionIds = map (^. M.id) (e ^. mentions)
        if not (e ^. isBot) && any (\m -> m == (botUser ^. U.id)) mentionIds
          then pingEventHandler res <|> organizeTimesHandler res <|> createChannelEventHandler res <|> helpEventHandler res
          else emptyEff
      Nothing -> emptyEff
  _ -> emptyEff
