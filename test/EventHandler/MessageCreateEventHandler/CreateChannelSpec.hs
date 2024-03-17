{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module EventHandler.MessageCreateEventHandler.CreateChannelSpec (
  spec
) where

import Test.Hspec
import Effectful.DiscordChannel.Effect hiding (roles) 
import EventHandler.MessageCreateEventHandler.CreateChannel 
import Data.Discord
import Data.Discord.Response.HelloEventResponse
import Effectful
import Effectful.DynamicLogger
import Effectful.NonDet
import Effectful.State.Static.Local
import Data.Either
import Effectful.Dispatch.Dynamic
import Data.Discord.Content
import Data.Discord.Response.MessageCreateEventResponse

runDummyDiscordChannel :: (State (Maybe CreateChannelParams) :> es) => Eff (DiscordChannel : es) a -> Eff es a
runDummyDiscordChannel = interpret $ \_ -> \case
  SendMessage _ -> pure ()
  CreateChannel  _ params -> put . Just $ params 


spec :: Spec

spec = describe "CreateChannel" $ do
  describe "createChannelEventHandler" $ do
    context "when not MessageCreate event provided" $ do
      it "should be return to emptyEff" $ do 
        let response =  Hello HelloEventResponse
        let actual = runPureEff . runSilentDynamicLogger . runNonDet OnEmptyKeep . runState @(Maybe CreateChannelParams) Nothing . runDummyDiscordChannel $ createChannelEventHandler response
        isLeft actual `shouldBe` True
    context "when MessageCreate event provided" $ do
      it "should be call create channel instruction" $ do 
        let msg = makeMessageCreateEventResponse (ChannelId "xxx") (makeUnsafeContent "ping") [] Member {roles = [], nick = Just . Nickname $ "himanoa"} True (GuildId "576648644942495744")
        let response =  MessageCreate msg
        let actual = runPureEff . runSilentDynamicLogger . runNonDet OnEmptyKeep . execState @(Maybe CreateChannelParams) Nothing . runDummyDiscordChannel $ createChannelEventHandler response
        isRight actual `shouldBe` True
