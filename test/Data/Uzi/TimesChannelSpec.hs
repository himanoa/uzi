{-# LANGUAGE OverloadedStrings #-}

module Data.Uzi.TimesChannelSpec (
  spec
) where

import Test.Hspec
import Data.Uzi.TimesChannel
import Data.Discord.Channel
import Data.Discord.ChannelName

spec :: Spec
spec = describe "TimesChannel" $ do
  describe "makeTimesChannel" $ do
    context "when channel type is voice" $ do
      it "should be return Nothing" $ do
        makeTimesChannel Channel { __id = ChannelId "xxx", __type = GuildVoice, __position = ChannelPosition 10, __name = ChannelName "yyy" } `shouldBe` Nothing

    context "when channel type is category" $ do
      it "should be return Nothing" $ do
        makeTimesChannel Channel { __id = ChannelId "xxx", __type = GuildCategory, __position = ChannelPosition 10, __name = ChannelName "yyy" } `shouldBe` Nothing

    context "when channel type is text" $ do
      context "when it is not times channel" $ do
        it "should be return Nothing " $ do
          makeTimesChannel Channel { __id = ChannelId "xxx", __type = GuildText, __position = ChannelPosition 10, __name = ChannelName "yyy" } `shouldBe` Nothing

      context "when channel name is start by times-" $ do
        it "should be return Justing TimesChannel object " $ do
          makeTimesChannel Channel { __id = ChannelId "xxx", __type = GuildText, __position = ChannelPosition 10, __name = ChannelName "times-name" } `shouldBe` Just TimesChannel { Data.Uzi.TimesChannel._id = ChannelId "xxx", Data.Uzi.TimesChannel._name = TimesName "name" }
          makeTimesChannel Channel { __id = ChannelId "xxx", __type = GuildText, __position = ChannelPosition 10, __name = ChannelName "times-name-foo" } `shouldBe` Just TimesChannel { Data.Uzi.TimesChannel._id = ChannelId "xxx", Data.Uzi.TimesChannel._name = TimesName "name-foo" }

      context "when channel name is timezunzun" $ do
        it "should be return Justing TimesChannel object " $ do
          makeTimesChannel Channel { __id = ChannelId "xxx", __type = GuildText, __position = ChannelPosition 10, __name = ChannelName "timezunzun" } `shouldBe` Just TimesChannel { Data.Uzi.TimesChannel._id = ChannelId "xxx", Data.Uzi.TimesChannel._name = TimesName "zunzun" }

      context "when channel name is the-ny-times" $ do
        it "should be return Justing TimesChannel object " $ do
          makeTimesChannel Channel { __id = ChannelId "xxx", __type = GuildText, __position = ChannelPosition 10, __name = ChannelName "the-ny-times" } `shouldBe` Just TimesChannel { Data.Uzi.TimesChannel._id = ChannelId "xxx", Data.Uzi.TimesChannel._name = TimesName "ny" }


