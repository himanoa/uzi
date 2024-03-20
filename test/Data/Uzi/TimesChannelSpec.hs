{-# LANGUAGE OverloadedStrings #-}

module Data.Uzi.TimesChannelSpec
  ( spec,
  )
where

import Data.Discord.Channel
import Data.Discord.ChannelName
import Data.List qualified as RIO.List
import Data.Uzi.TimesChannel qualified as TC
import RIO qualified
import RIO.Vector qualified as RV
import Test.Hspec

spec :: Spec
spec = describe "TimesChannel" $ do
  describe "TC.makeTimesChannel" $ do
    context "when channel type is voice" $ do
      it "should be return Nothing" $ do
        TC.makeTimesChannel Channel {__id = ChannelId "xxx", __type = GuildVoice, __position = ChannelPosition 10, __name = ChannelName "yyy"} `shouldBe` Nothing

    context "when channel type is category" $ do
      it "should be return Nothing" $ do
        TC.makeTimesChannel Channel {__id = ChannelId "xxx", __type = GuildCategory, __position = ChannelPosition 10, __name = ChannelName "yyy"} `shouldBe` Nothing

    context "when channel type is text" $ do
      context "when it is not times channel" $ do
        it "should be return Nothing " $ do
          TC.makeTimesChannel Channel {__id = ChannelId "xxx", __type = GuildText, __position = ChannelPosition 10, __name = ChannelName "yyy"} `shouldBe` Nothing

      context "when channel name is start by times-" $ do
        it "should be return Justing TimesChannel object " $ do
          TC.makeTimesChannel Channel {__id = ChannelId "xxx", __type = GuildText, __position = ChannelPosition 10, __name = ChannelName "times-name"} `shouldBe` Just TC.TimesChannel {TC._id = ChannelId "xxx", TC._name = TC.TimesName "name"}
          TC.makeTimesChannel Channel {__id = ChannelId "xxx", __type = GuildText, __position = ChannelPosition 10, __name = ChannelName "times-name-foo"} `shouldBe` Just TC.TimesChannel {TC._id = ChannelId "xxx", TC._name = TC.TimesName "name-foo"}

      context "when channel name is timezunzun" $ do
        it "should be return Justing TimesChannel object " $ do
          TC.makeTimesChannel Channel {__id = ChannelId "xxx", __type = GuildText, __position = ChannelPosition 10, __name = ChannelName "timezunzun"} `shouldBe` Just TC.TimesChannel {TC._id = ChannelId "xxx", TC._name = TC.TimesName "zunzun"}

      context "when channel name is the-ny-times" $ do
        it "should be return Justing TimesChannel object " $ do
          TC.makeTimesChannel Channel {__id = ChannelId "xxx", __type = GuildText, __position = ChannelPosition 10, __name = ChannelName "the-ny-times"} `shouldBe` Just TC.TimesChannel {TC._id = ChannelId "xxx", TC._name = TC.TimesName "ny"}

  describe "fromChannels" $ do
    context "when input is empty vec" $ do
      it "should be return empty vec" $ do
        TC.fromChannels (RIO.mempty @(RIO.Vector Channel)) `shouldBe` (RIO.mempty @(RIO.Vector TC.TimesChannel))

    context "when the input is consists exclusively of voice channels" $ do
      it "should be return empty vec" $ do
        let channel = Channel {__id = ChannelId "", __type = GuildVoice, __position = ChannelPosition 1, __name = ChannelName "xxxx"}
        TC.fromChannels (RV.singleton channel) `shouldBe` (RIO.mempty @(RIO.Vector TC.TimesChannel))

  describe "Ord" $ do
    it "should be return to a,b,c" $ do
      let a = TC.TimesChannel {TC._id = ChannelId "xxx", TC._name = TC.TimesName "a"}
      let b = TC.TimesChannel {TC._id = ChannelId "xxx", TC._name = TC.TimesName "b"}
      let c = TC.TimesChannel {TC._id = ChannelId "xxx", TC._name = TC.TimesName "c"}
      RIO.List.sort [b, c, a] `shouldBe` [a, b, c]
