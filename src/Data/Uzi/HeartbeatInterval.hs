{-|
 Module: Data.Uzi.HeartbeatInterval
 Description: Discordに送るHeartbeatの間隔についての定義
 Maintainer: himanoa <matsunoappy@gmail.com>

 Discordのハートビートリクエストを送る間隔を保存するためのデータ構造です。
 ハートビートの間隔は DiscordGatewayAPIのHelloEventによって受信し、'State HeartbeatInterval' に保存します。

 State HeartbeatInterval を読みこんで間隔通りにHeartbeatリクエストを送るのは 'Lib.sendHeartbeat' です

 DiscordAPI側のドキュメント: https://discord.com/developers/docs/topics/voice-connections#heartbeating
|-}
module Data.Uzi.HeartbeatInterval
  ( HeartbeatInterval (..),
    coerceHeartbeatInterval,
    makeHeartbeatInterval,
  )
where

import Data.Coerce (coerce)
import RIO

newtype HeartbeatInterval = HeartbeatInterval Int
  deriving (Show, Eq)

coerceHeartbeatInterval :: HeartbeatInterval -> Int
coerceHeartbeatInterval = coerce

makeHeartbeatInterval :: Int -- | HeartbeatIntervalのミリ秒
  -> HeartbeatInterval
makeHeartbeatInterval = HeartbeatInterval
