{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- |
-- Module: EventHandler.MessageCreateEventHandler.Help
-- Description: Discordで'help'メッセージコマンドを処理するイベントハンドラー。
-- Maintainer: himanoa <matsunoappy@gmail.com>
--
-- このモジュールには、Discordチャンネルで'help'メッセージコマンドに応答するイベントハンドラーが含まれています。
-- ヘルプドキュメントへのリンクを提供します。
module EventHandler.MessageCreateEventHandler.Help where

import Control.Lens
import Data.Discord
import Data.Discord.Content (body)
import Data.Discord.Response.InteractionCreateEventResponse qualified as IC
import Data.Either.Validation
import Effectful
import Effectful.DiscordChannel
import Effectful.DynamicLogger
import Effectful.NonDet
import RIO hiding ((^.))

-- | 'MessageCreate'イベントを処理し、メッセージ内容が'help'かどうかを確認します。
-- そうであれば、ヘルプドキュメントへのリンクを含むメッセージを送信します。
--
-- この関数は、さまざまなタイプのイベントを処理するためにケース式を使用しており、
-- 特に'MessageCreate'に焦点を当てています。メッセージ本体が'help'を含んでいるかどうかを確認します。
-- 成功した場合、ディスパッチをログに記録し、ヘルプURLを含むメッセージを構築し、送信します。
-- URLをコンテンツオブジェクトに変換できない場合は何もしません。
helpEventHandler :: (DiscordChannel :> es, NonDet :> es, DynamicLogger :> es) => Response -> Eff es ()
helpEventHandler = \case
  InteractionCreate event -> do
    if (event ^. IC.slashCommandName) == "help"
      then case makeContent "https://github.com/himanoa/uzi/blob/master/docs/HELP.md" of
        Success c -> do
          info "Dispatched Help Handler"
          let params = makeSendMessageParams (event ^. IC.channelId) c Nothing False Nothing Nothing Nothing
          sendMessage params
          pure ()
        Failure _ -> pure ()
      else emptyEff
    pure ()
  _ -> emptyEff
