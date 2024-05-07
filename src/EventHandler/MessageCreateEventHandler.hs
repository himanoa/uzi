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

import Data.Discord
import Effectful
import Effectful.DiscordChannel
import Effectful.DynamicLogger
import Effectful.NonDet
import EventHandler.MessageCreateEventHandler.CreateChannel (createChannelEventHandler)
import EventHandler.MessageCreateEventHandler.Help
import EventHandler.MessageCreateEventHandler.OrganizeTimes
import EventHandler.MessageCreateEventHandler.Ping

-- | メッセージが投稿された時に実行されるイベントハンドラです
--
-- 内部で実行したいコマンドごとにハンドラを分けるためにNonDet Effectに依存しています
dispatchMessageEventHandlers :: (DiscordChannel :> es, NonDet :> es, DynamicLogger :> es) => Response -> Eff es ()
dispatchMessageEventHandlers res = case res of
  InteractionCreate _ ->
    pingEventHandler res <|> organizeTimesHandler res <|> createChannelEventHandler res <|> helpEventHandler res
  _ -> emptyEff
