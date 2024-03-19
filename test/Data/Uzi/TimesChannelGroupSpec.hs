{-# LANGUAGE OverloadedStrings #-}

module Data.Uzi.TimesChannelGroupSpec
  ( spec,
  )
where

import Data.Discord.Channel qualified as C
import Data.Discord.ChannelName
import Data.Uzi.TimesChannelGroup
import RIO qualified
import RIO.Vector qualified as RIOV
import Test.Hspec

spec :: Spec
spec = describe "TimesChannelGroup" $ do
  describe "findTimesCategories" $ do
    context "when empty vec provided" $ do
      it "should be return to Left AllMissing" $ do
        findTimesCategories RIOV.empty `shouldBe` Left AllMissing

    context "when vec include only aToM group" $ do
      it "should be return to Left NtoZGroupMissing" $ do
        let cs =
              RIOV.fromList
                [ C.Channel
                    { C.__id = C.ChannelId "x",
                      C.__type = C.GuildCategory,
                      C.__position = C.ChannelPosition 0,
                      C.__name = ChannelName "TIMES(A-M)"
                    }
                ]
        findTimesCategories cs `shouldBe` Left NtoZGroupMissing

    context "when vec include only nToZ group" $ do
      it "should be return to Left NtoZGroupMissing" $ do
        let cs =
              RIOV.fromList
                [ C.Channel
                    { C.__id = C.ChannelId "x",
                      C.__type = C.GuildCategory,
                      C.__position = C.ChannelPosition 0,
                      C.__name = ChannelName "TIMES(N-Z)"
                    }
                ]
        findTimesCategories cs `shouldBe` Left AtoMGroupMissing

    context "when vec includes aToM and nToZ group" $ do
      it "should be return to Left NtoZGroupMissing" $ do
        let cs =
              RIOV.fromList
                [ C.Channel
                    { C.__id = C.ChannelId "x",
                      C.__type = C.GuildCategory,
                      C.__position = C.ChannelPosition 0,
                      C.__name = ChannelName "TIMES(N-Z)"
                    },
                  C.Channel
                    { C.__id = C.ChannelId "y",
                      C.__type = C.GuildCategory,
                      C.__position = C.ChannelPosition 0,
                      C.__name = ChannelName "TIMES(A-M)"
                    }
                ]
        RIO.isRight (findTimesCategories cs) `shouldBe` True
