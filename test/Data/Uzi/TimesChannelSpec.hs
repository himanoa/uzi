{-# LANGUAGE OverloadedStrings #-}

module Data.Uzi.TimesChannelSpec
  ( spec,
  )
where

import Data.Discord.Channel
import Data.Discord.ChannelName
import Data.Uzi.TimesChannel
import RIO qualified
import RIO.Vector qualified as RV
import Test.Hspec

spec :: Spec
spec = describe "TimesChannel" $ do
  describe "makeTimesChannel" $ do
    context "when channel type is voice" $ do
      it "should be return Nothing" $ do
        makeTimesChannel Channel {__id = ChannelId "xxx", __type = GuildVoice, __position = ChannelPosition 10, __name = ChannelName "yyy"} `shouldBe` Nothing

    context "when channel type is category" $ do
      it "should be return Nothing" $ do
        makeTimesChannel Channel {__id = ChannelId "xxx", __type = GuildCategory, __position = ChannelPosition 10, __name = ChannelName "yyy"} `shouldBe` Nothing

    context "when channel type is text" $ do
      context "when it is not times channel" $ do
        it "should be return Nothing " $ do
          makeTimesChannel Channel {__id = ChannelId "xxx", __type = GuildText, __position = ChannelPosition 10, __name = ChannelName "yyy"} `shouldBe` Nothing

      context "when channel name is start by times-" $ do
        it "should be return Justing TimesChannel object " $ do
          makeTimesChannel Channel {__id = ChannelId "xxx", __type = GuildText, __position = ChannelPosition 10, __name = ChannelName "times-name"} `shouldBe` Just TimesChannel {Data.Uzi.TimesChannel._id = ChannelId "xxx", Data.Uzi.TimesChannel._name = TimesName "name"}
          makeTimesChannel Channel {__id = ChannelId "xxx", __type = GuildText, __position = ChannelPosition 10, __name = ChannelName "times-name-foo"} `shouldBe` Just TimesChannel {Data.Uzi.TimesChannel._id = ChannelId "xxx", Data.Uzi.TimesChannel._name = TimesName "name-foo"}

      context "when channel name is timezunzun" $ do
        it "should be return Justing TimesChannel object " $ do
          makeTimesChannel Channel {__id = ChannelId "xxx", __type = GuildText, __position = ChannelPosition 10, __name = ChannelName "timezunzun"} `shouldBe` Just TimesChannel {Data.Uzi.TimesChannel._id = ChannelId "xxx", Data.Uzi.TimesChannel._name = TimesName "zunzun"}

      context "when channel name is the-ny-times" $ do
        it "should be return Justing TimesChannel object " $ do
          makeTimesChannel Channel {__id = ChannelId "xxx", __type = GuildText, __position = ChannelPosition 10, __name = ChannelName "the-ny-times"} `shouldBe` Just TimesChannel {Data.Uzi.TimesChannel._id = ChannelId "xxx", Data.Uzi.TimesChannel._name = TimesName "ny"}

  describe "makeTimesChannels" $ do
    context "when input is empty vec" $ do
      it "should be return empty vec" $ do
        makeTimesChannels (RIO.mempty @(RIO.Vector Channel)) `shouldBe` (RIO.mempty @(RIO.Vector TimesChannel))

    context "when the input is consists exclusively of voice channels" $ do
      it "should be return empty vec" $ do
        let channel = Channel {__id = ChannelId "", __type = GuildVoice, __position = ChannelPosition 1, __name = ChannelName "xxxx"}
        makeTimesChannels (RV.singleton channel) `shouldBe` (RIO.mempty @(RIO.Vector TimesChannel))
