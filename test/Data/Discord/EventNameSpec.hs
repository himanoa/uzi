{-# LANGUAGE OverloadedStrings #-}

module Data.Discord.EventNameSpec (spec) where

import Data.Aeson
import Data.Discord
import Test.Hspec

spec :: Spec
spec = describe "EventName" $ do
  describe "FromJSON" $ do
    describe "parseJSON" $ do
      describe "if READY" $ do
        it "is return to ReadyEventName" $ do
          decode @EventName "\"READY\"" `shouldBe` Just ReadyEventName
      describe "if MESSAGE_CREATE" $ do
        it "is return to ReadyEventName" $ do
          decode @EventName "\"MESSAGE_CREATE\"" `shouldBe` Just MessageCreateEventName
      describe "if GUILD_CREATE" $ do
        it "is return to GuildCreateEventName" $ do
          decode @EventName "\"GUILD_CREATE\"" `shouldBe` Just GuildCreateEventName
      describe "else" $ do
        it "is return to Nothing" $ do
          decode @EventName "\"UNSUPPORTED\"" `shouldBe` Nothing
