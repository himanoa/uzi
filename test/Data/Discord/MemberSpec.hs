{-# LANGUAGE OverloadedStrings #-}

module Data.Discord.MemberSpec
  ( spec,
  )
where

import Data.Aeson (decode)
import Data.Discord
import Test.Hspec

spec :: Spec
spec = describe "Member" $ do
  describe "FromJSON" $ do
    describe "parseJSON" $ do
      context "if nick is null" $ do
        it "should be return to Member" $ do
          let json = "{\"roles\": [], \"nick\": null}"
          decode @Member json `shouldBe` Just Member {roles = [], nick = Nothing}
      context "if nick is himanoa" $ do
        it "should be return to Member" $ do
          let json = "{\"roles\": [], \"nick\": \"himanoa\"}"
          decode @Member json `shouldBe` Just Member {roles = [], nick = Just . Nickname $ "himanoa"}
      context "if role is not empty" $ do
        it "should be return to Member" $ do
          let json = "{\"roles\": [\"xxxx\"], \"nick\": \"himanoa\"}"
          decode @Member json `shouldBe` Just Member {roles = [Role "xxxx"], nick = Just . Nickname $ "himanoa"}
