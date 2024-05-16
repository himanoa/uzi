{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module EventHandler.MessageCreateEventHandler.OrganizeTimesSpec
  ( spec,
  )
where

import Data.Discord
import Data.Discord.Response.HelloEventResponse
import Data.Discord.Response.InteractionCreateEventResponse
  ( InteractionId (..),
    InteractionToken (..),
    makeInteractionCreateEventResponse,
  )
import Data.Either
import Data.Map qualified as M
import Effectful
import Effectful.DiscordChannel.Effect hiding (roles)
import Effectful.Dispatch.Dynamic
import Effectful.DynamicLogger
import Effectful.InteractionCallback.Effect
import Effectful.NonDet
import Effectful.State.Static.Local
import EventHandler.MessageCreateEventHandler.OrganizeTimes
import Test.Hspec

runDummyDiscordChannel :: (State (Maybe CreateChannelParams) :> es) => Eff (DiscordChannel : es) a -> Eff es a
runDummyDiscordChannel = interpret $ \_ -> \case
  SendMessage _ -> pure ()
  CreateChannel _ params -> put . Just $ params
  GetChannels _ -> pure []
  ModifyChannel {} -> pure ()

runDummyInteractionCallback :: Eff (InteractionCallback : es) a -> Eff es a
runDummyInteractionCallback = interpret $ \_ -> \case
  ChannelMessage _ _ _ -> do
    pure ()
  Loading _ -> do
    pure ()

spec :: Spec
spec = describe "OrganizeTimes" $ do
  describe "organizeTimesHandler" $ do
    context "when not MessageCreate event provided" $ do
      it "should be return to emptyEff" $ do
        let response = Hello HelloEventResponse {_interval = 10}
        let actual = runPureEff . runSilentDynamicLogger . runNonDet OnEmptyKeep . execState @(Maybe CreateChannelParams) Nothing . runDummyInteractionCallback . runDummyDiscordChannel $ organizeTimesHandler response
        isLeft actual `shouldBe` True
    context "when MessageCreate event provided" $ do
      it "should be call create channel instruction" $ do
        let interactionId = InteractionId "1237511116112924732"
        let interactionToken = InteractionToken "aW50ZXJhY3Rpb246MTIzNzUxMTExNjExMjkyNDczMjptb01HV0JlU3U2Z29veFRuaWVDYkpSQWhuQkRkQlFsVXdqRXJpMjlaYWJ0WDFWdlBGekU2bVBMUkRvVlNjZEx5TEltNXhhNU1Id0ZBdFVxYnprWmFqNjhqSzdkZmVkOGw1bnhidlloVnJwblpIU3hWRmRvbDZXWHhYN2RPbGtrTg"
        let msg = makeInteractionCreateEventResponse (ChannelId "xxx") Member {roles = [], nick = Just . Nickname $ "himanoa"} "organize-times" M.empty (GuildId "576648644942495744") interactionId interactionToken
        let response = InteractionCreate msg
        let actual = runPureEff . runSilentDynamicLogger . runNonDet OnEmptyKeep . execState @(Maybe CreateChannelParams) Nothing . runDummyInteractionCallback . runDummyDiscordChannel $ organizeTimesHandler response
        isRight actual `shouldBe` True
