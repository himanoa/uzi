{-# LANGUAGE OverloadedStrings #-}
module Data.Discord.MemberSpec (
  spec
) where
import Test.Hspec
import Data.Aeson (decode)
import Data.Discord.Member
import Data.Discord

spec::Spec
spec = describe "Member" $ do
  describe "FromJSON" $ do
    describe "parseJSON" $ do
      context "if nick is null" $ do
        it "should be return to Member" $ do
          let json = "{\"roles\": [], \"nick\": null}"
          decode @Member json `shouldBe` Just Member { roles = [], nick = Nothing }
    describe "parseJSON" $ do
      context "if nick is himanoa" $ do
        it "should be return to Member" $ do
          let json = "{\"roles\": [], \"nick\": \"himanoa\"}"
          decode @Member json `shouldBe` Just Member { roles = [], nick = Just . Nickname $ "himanoa" }
