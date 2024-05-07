{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module EventHandler.MessageCreateEventHandler.OrganizeTimesSpec
  ( spec,
  )
where

import Data.Discord
import Data.Discord.Content
import Data.Discord.Response.HelloEventResponse
import Data.Discord.Response.InteractionCreateEventResponse (makeInteractionCreateEventResponse)
import Data.Discord.Response.MessageCreateEventResponse
import Data.Either
import Data.Map qualified as M
import Effectful
import Effectful.DiscordChannel.Effect hiding (roles)
import Effectful.Dispatch.Dynamic
import Effectful.DynamicLogger
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

spec :: Spec
spec = describe "OrganizeTimes" $ do
  describe "organizeTimesHandler" $ do
    context "when not MessageCreate event provided" $ do
      it "should be return to emptyEff" $ do
        let response = Hello HelloEventResponse {_interval = 10}
        let actual = runPureEff . runSilentDynamicLogger . runNonDet OnEmptyKeep . runState @(Maybe CreateChannelParams) Nothing . runDummyDiscordChannel $ organizeTimesHandler response
        isLeft actual `shouldBe` True
    context "when MessageCreate event provided" $ do
      it "should be call create channel instruction" $ do
        let msg = makeInteractionCreateEventResponse (ChannelId "xxx") Member {roles = [], nick = Just . Nickname $ "himanoa"} "organize-times" M.empty (GuildId "576648644942495744")
        let response = InteractionCreate msg
        let actual = runPureEff . runSilentDynamicLogger . runNonDet OnEmptyKeep . execState @(Maybe CreateChannelParams) Nothing . runDummyDiscordChannel $ organizeTimesHandler response
        isRight actual `shouldBe` True
