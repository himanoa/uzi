{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

{-|
 Module: Effectful.DiscordApiTokenReader.Effect
 Description: UziBotが使うDiscordのアクセストークンを取得するEffect
 Maintainer: himanoa <matsunoappy@gmail.com>

 UziBotが使うDiscordのアクセストークンを読み出すためのEffect Moduleです

-}
module Effectful.DiscordApiTokenReader.Effect where

import Data.Text
import Effectful
import Effectful.Dispatch.Dynamic (send)

-- | UziBotが使うDiscordのアクセストークンを読み出すためのEffectの定義
data DiscordApiTokenReader :: Effect where
  GetToken :: DiscordApiTokenReader m Text

type instance DispatchOf DiscordApiTokenReader = Dynamic

-- | DiscordAPIにアクセスするためのToken文字列を取得します。
--
-- このAPIはEventHandlerなどからは直接読み出さないでください。
--
-- DiscordのAPIを使いたい場合は 'Effectful.DiscordChannel' や 'Effectful.DiscordGateway' などのこれらをラップしたEffectを使用してください。
getToken :: (DiscordApiTokenReader :> es) => Eff es Text
getToken = send GetToken
