{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- |
-- Module: EventHandler.MessageCreateEventHandler.OrganizeTimes
-- Description : Discordで'times channel'の整理を行うイベントハンドラー。
-- Maintainer: himanoa <matsunoappy@gmail.com>
--
-- このモジュールには、Discordチャンネルの'message create'イベントに対応し、特定のコマンド('organize-times')が
-- 送信された際にtimes channelを整理する処理が含まれています。
module EventHandler.MessageCreateEventHandler.OrganizeTimes where

import Control.Lens
import Data.Discord
import Data.Discord.Content
import Data.Discord.Response.InteractionCreateEventResponse qualified as IC
import Data.Uzi.OrganizeTimes
import Effectful
import Effectful.DiscordChannel
import Effectful.DynamicLogger
import Effectful.Error.Dynamic
import Effectful.NonDet
import RIO hiding ((^.))
import RIO.Text qualified as T

-- | 'MessageCreate'イベントに反応し、特定のメッセージ('organize-times')を受け取った際にtimes channelの整理を行います。
-- 成功すると成功のメッセージを、失敗するとエラーメッセージを送信します。
--
-- この関数は'MessageCreate'イベントを処理し、受け取ったメッセージが'organize-times'かどうかを確認します。
-- 条件を満たす場合、整理プロセスを開始し、成功または失敗のログを記録し、対応するメッセージをチャンネルに送信します。
-- 整理プロセス中に発生したエラーは適切にハンドリングされ、エラーメッセージが送信されます。
organizeTimesHandler :: (DiscordChannel :> es, NonDet :> es, DynamicLogger :> es) => Response -> Eff es ()
organizeTimesHandler = \case
  InteractionCreate event -> do
    if (event ^. IC.slashCommandName) == "organize-times"
      then do
        info "organizeTimesHandler dispatched"
        sendMessage (makeMessage (event ^. IC.channelId) (makeUnsafeContent "times channelの整理を開始したよ！"))
        organizeTimesEither <- runError @OrganizeTimesError (organizeTimes (event ^. IC.guildId))
        case organizeTimesEither of
          Right _ -> do
            info "Organized times"
            sendMessage (makeMessage (event ^. IC.channelId) (makeUnsafeContent "times channelを整理したよ！"))
          Left (_, e) -> do
            attention "Failed organize times"
            sendMessage (makeMessage (event ^. IC.channelId) (makeUnsafeContent ("times channelの整理に失敗したよ！ " <> (T.pack . show $ e))))
      else emptyEff
  _ -> emptyEff
