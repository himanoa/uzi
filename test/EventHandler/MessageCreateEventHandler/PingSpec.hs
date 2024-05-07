{-# LANGUAGE OverloadedStrings #-}

module EventHandler.MessageCreateEventHandler.PingSpec
  ( spec,
  )
where

import Control.Lens
import Data.Discord
import Data.Discord.Content
import Data.Discord.Response.HelloEventResponse
import Data.Discord.Response.InteractionCreateEventResponse (makeInteractionCreateEventResponse)
import Data.Discord.Response.MessageCreateEventResponse hiding (content)
import Data.Either
import Data.Map qualified as M
import Data.Maybe
import Effectful
import Effectful.DiscordChannel.Effect hiding (roles)
import Effectful.DynamicLogger
import Effectful.NonDet
import Effectful.State.Static.Local
import EventHandler.MessageCreateEventHandler.Ping (pingEventHandler)
import Helper.DummyDiscordChannelInterpreter
import Test.Hspec

spec :: Spec
spec = describe "PingSpec" $ do
  describe "pingEventHandler" $ do
    context "when provide not MessageCreateEvent" $ do
      it "should be return to emptyEff" $ do
        let response = Hello HelloEventResponse {_interval = 10}
        let actual = runPureEff . runSilentDynamicLogger . runNonDet OnEmptyKeep . runState @(Maybe SendMessageParams) Nothing . runDummyDiscordChannel $ pingEventHandler response
        isLeft actual `shouldBe` True
    context "when provide MessageCreateEvent" $ do
      context "when response message is ping" $ do
        it "should be return to pong response" $ do
          let response = makeInteractionCreateEventResponse (ChannelId "xxx") Member {roles = [], nick = Just . Nickname $ "himanoa"} "ping" M.empty (GuildId "576648644942495744")
          let actual = runPureEff . runSilentDynamicLogger . runNonDet OnEmptyKeep . runState @(Maybe SendMessageParams) Nothing . runDummyDiscordChannel . pingEventHandler . InteractionCreate $ response
          let (_, paramsMaybe) = fromRight ((), Nothing) actual
          let params = fromJust paramsMaybe

          (params ^. content) `shouldBe` makeUnsafeContent "pong"

      context "when response message is not ping" $ do
        it "should be return to pong response" $ do
          let response = makeInteractionCreateEventResponse (ChannelId "xxx") Member {roles = [], nick = Just . Nickname $ "himanoa"} "dummy" M.empty (GuildId "576648644942495744")
          let actual = runPureEff . runSilentDynamicLogger . runNonDet OnEmptyKeep . runState @(Maybe SendMessageParams) Nothing . runDummyDiscordChannel . pingEventHandler . InteractionCreate $ response

          isLeft actual `shouldBe` True
