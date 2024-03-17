{-# LANGUAGE OverloadedStrings #-}

module Data.Discord.ContentSpec
  ( spec,
  )
where

import Data.Discord.Content
import Test.Hspec

spec :: Spec
spec = describe "Content" $ do
  describe "body" $ do
    context "when not include mention symbol" $ do
      it "should be return to foo" $ do
        (body . makeUnsafeContent $ "foo") `shouldBe` Just "foo"
      it "should be exclude mention block " $ do
        (body . makeUnsafeContent $ "<123123123> foo") `shouldBe` Just "<123123123> foo"

    context "when include mention symbol" $ do
      it "should be exclude mention block " $ do
        (body . makeUnsafeContent $ "<@123123123> foo") `shouldBe` Just "foo"

    context "when includes multiple mention symbols" $ do
      it "should be exclude mention block " $ do
        (body . makeUnsafeContent $ "<@123123123> <@124123112> foo") `shouldBe` Just "foo"
        (body . makeUnsafeContent $ "<@123123123> <@124123112> foo bar <@123123>") `shouldBe` Just "foo bar"
