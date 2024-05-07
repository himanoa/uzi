{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- |
-- Module: EventHandler.MessageCreateEventHandler.CreateChannel
-- Description: Discordで'times channel'を作成するイベントハンドラー。
-- Maintainer: himanoa <matsunoappy@gmail.com>
--
-- このモジュールには、Discordチャンネルの'message create'イベントに対応し、特定のコマンド('create-times')が
-- 送信された際に新しいtimes channelを作成する処理が含まれています。さらに、作成後にそのチャンネルを整理する機能も提供します。
module EventHandler.MessageCreateEventHandler.CreateChannel where

import Control.Lens
import Data.Discord
import Data.Discord.Content
import Data.Discord.Response.InteractionCreateEventResponse qualified as IC
import Data.Map.Strict qualified as M
import Data.Text
import Data.Uzi.OrganizeTimes
import Effectful
import Effectful.DiscordChannel
import Effectful.DynamicLogger
import Effectful.Error.Dynamic
import Effectful.NonDet
import RIO hiding ((^.))

-- | 'MessageCreate'イベントに反応し、特定のメッセージ('create-times')を受け取った際に新しいtimes channelの作成を行います。
-- 作成後、そのチャンネルを整理する処理も実施します。処理が成功すると、成功のメッセージを、失敗するとエラーメッセージを送信します。
--
-- この関数は'MessageCreate'イベントを処理し、受け取ったメッセージが'create-times'コマンドであるかどうかを解析します。
-- 条件を満たす場合、新しいtimes channelを作成し、成功または失敗のログを記録し、対応するメッセージをチャンネルに送信します。
-- その後、チャンネルを整理する処理が行われ、その結果に基づいて追加のメッセージが送信されます。
createChannelEventHandler :: (DiscordChannel :> es, NonDet :> es, DynamicLogger :> es) => Response -> Eff es ()
createChannelEventHandler = \case
  InteractionCreate res -> do
    case res ^. IC.slashCommandName of
      "create-times" -> do
        let params = res ^. IC.commandOptions
        let maybeName = M.lookup ("name" :: Text) params
        case maybeName of
          Nothing -> emptyEff
          Just name -> do
            let guildId = res ^. IC.guildId
            info "CreateChannelEventHandler dispatched"
            createChannel guildId (makeCreateChannelParams . ChannelName $ "times-" <> name)
            sendMessage (makeMessage (res ^. IC.channelId) (makeUnsafeContent ("timesを作ったよ -> #times-" <> name)))
            _ <-
              (runError @OrganizeTimesError . organizeTimes $ guildId) >>= \case
                Right _ -> sendMessage (makeMessage (res ^. IC.channelId) (makeUnsafeContent "times channelをソートしたよ"))
                Left _ -> pure ()
            pure ()
      _ -> emptyEff
  _ -> emptyEff
