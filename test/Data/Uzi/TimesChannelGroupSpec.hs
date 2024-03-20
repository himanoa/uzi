{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Uzi.TimesChannelGroupSpec
  ( spec,
  )
where

import Data.Discord.Channel qualified as C
import Data.Discord.ChannelId qualified as C
import Data.Discord.ChannelName
import Data.Uzi.TimesChannel qualified as TC
import Data.Uzi.TimesChannelGroup
import RIO qualified
import RIO.Map qualified as Map
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

  describe "groupByFirstLetter" $ do
    let aToMGroup = AtoMGroup (C.ChannelId "aToM")
    let nToZGroup = NtoZGroup (C.ChannelId "nToZ")

    context "when channel name is started by a" $ do
      let channel =
            TC.TimesChannel
              { TC._id = C.ChannelId "aToM",
                TC._name = TC.TimesName "a"
              }

      it "should be belonging to aToM group" $ do
        groupByFirstLetter [channel] aToMGroup nToZGroup `shouldBe` Map.fromList [(aToMGroup, [channel])]

    context "when channel name is started by m" $ do
      let channel =
            TC.TimesChannel
              { TC._id = C.ChannelId "aToM",
                TC._name = TC.TimesName "m"
              }

      it "should be belonging to aToM group" $ do
        groupByFirstLetter [channel] aToMGroup nToZGroup `shouldBe` Map.fromList [(aToMGroup, [channel])]

    context "when channel name is started by n" $ do
      let channel =
            TC.TimesChannel
              { TC._id = C.ChannelId "nToZ",
                TC._name = TC.TimesName "n"
              }

      it "should be belonging to nToZ group" $ do
        groupByFirstLetter [channel] aToMGroup nToZGroup `shouldBe` Map.fromList [(nToZGroup, [channel])]

    context "when channel name is started by z" $ do
      let channel =
            TC.TimesChannel
              { TC._id = C.ChannelId "nToZ",
                TC._name = TC.TimesName "z"
              }

      it "should be belonging to nToZ group" $ do
        groupByFirstLetter [channel] aToMGroup nToZGroup `shouldBe` Map.fromList [(nToZGroup, [channel])]

    context "when two of the same kind" $ do
      let channel1 =
            TC.TimesChannel
              { TC._id = C.ChannelId "nToZ",
                TC._name = TC.TimesName "z"
              }

      let channel2 =
            TC.TimesChannel
              { TC._id = C.ChannelId "nToZ2",
                TC._name = TC.TimesName "x"
              }

      it "should be belonging to nToZ group" $ do
        groupByFirstLetter [channel1, channel2] aToMGroup nToZGroup `shouldBe` Map.fromList [(nToZGroup, [channel2, channel1])]

    context "when two of the different kind" $ do
      let channel1 =
            TC.TimesChannel
              { TC._id = C.ChannelId "nToZ",
                TC._name = TC.TimesName "z"
              }

      let channel2 =
            TC.TimesChannel
              { TC._id = C.ChannelId "aToZ",
                TC._name = TC.TimesName "a"
              }

      it "should be belonging to nToZ group" $ do
        groupByFirstLetter [channel1, channel2] aToMGroup nToZGroup `shouldBe` Map.fromList [(nToZGroup, [channel1]), (aToMGroup, [channel2])]
