{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module EventHandler.MessageCreateEventHandler.PingSpec
  ( spec,
  )
where

import Data.Discord
import Data.Discord.Content
import Data.Discord.Response.HelloEventResponse
import Data.Discord.Response.InteractionCreateEventResponse (InteractionId (..), InteractionToken (..), makeInteractionCreateEventResponse)
import Data.Either
import Data.Map qualified as M
import Data.Maybe
import Effectful
import Effectful.DynamicLogger
import Effectful.NonDet
import Effectful.State.Static.Local
import EventHandler.MessageCreateEventHandler.Ping (pingEventHandler)
import Helper.DummyDiscordInteractionCallback
import Test.Hspec

spec :: Spec
spec = describe "PingSpec" $ do
  describe "pingEventHandler" $ do
    context "when provide not MessageCreateEvent" $ do
      it "should be return to emptyEff" $ do
        let response = Hello HelloEventResponse {_interval = 10}
        let actual = runPureEff . runSilentDynamicLogger . runNonDet OnEmptyKeep . runState @(Maybe Content) Nothing . runDummyInteractionCallback $ pingEventHandler response
        isLeft actual `shouldBe` True
    context "when provide MessageCreateEvent" $ do
      context "when response message is ping" $ do
        it "should be return to pong response" $ do
          let interactionId = InteractionId "1237511116112924732"
          let interactionToken = InteractionToken "aW50ZXJhY3Rpb246MTIzNzUxMTExNjExMjkyNDczMjptb01HV0JlU3U2Z29veFRuaWVDYkpSQWhuQkRkQlFsVXdqRXJpMjlaYWJ0WDFWdlBGekU2bVBMUkRvVlNjZEx5TEltNXhhNU1Id0ZBdFVxYnprWmFqNjhqSzdkZmVkOGw1bnhidlloVnJwblpIU3hWRmRvbDZXWHhYN2RPbGtrTg"
          let response = makeInteractionCreateEventResponse (ChannelId "xxx") Member {roles = [], nick = Just . Nickname $ "himanoa"} "ping" M.empty (GuildId "576648644942495744") interactionId interactionToken
          let actual = runPureEff . runSilentDynamicLogger . runNonDet OnEmptyKeep . runState @(Maybe Content) Nothing . runDummyInteractionCallback . pingEventHandler . InteractionCreate $ response
          let (_, paramsMaybe) = fromRight ((), Nothing) actual
          let params = fromJust paramsMaybe

          params `shouldBe` makeUnsafeContent "pong"

      context "when response message is not ping" $ do
        it "should be return to pong response" $ do
          let interactionId = InteractionId "1237511116112924732"
          let interactionToken = InteractionToken "aW50ZXJhY3Rpb246MTIzNzUxMTExNjExMjkyNDczMjptb01HV0JlU3U2Z29veFRuaWVDYkpSQWhuQkRkQlFsVXdqRXJpMjlaYWJ0WDFWdlBGekU2bVBMUkRvVlNjZEx5TEltNXhhNU1Id0ZBdFVxYnprWmFqNjhqSzdkZmVkOGw1bnhidlloVnJwblpIU3hWRmRvbDZXWHhYN2RPbGtrTg"

          let response = makeInteractionCreateEventResponse (ChannelId "xxx") Member {roles = [], nick = Just . Nickname $ "himanoa"} "dummy" M.empty (GuildId "576648644942495744") interactionId interactionToken
          let actual = runPureEff . runSilentDynamicLogger . runNonDet OnEmptyKeep . runState @(Maybe Content) Nothing . runDummyInteractionCallback . pingEventHandler . InteractionCreate $ response

          isLeft actual `shouldBe` True
