{-# LANGUAGE OverloadedStrings #-}

module Data.Discord.Response.HelloEventResponseSpec
  ( spec,
  )
where

import Data.Aeson (decode)
import Data.Discord.Response.HelloEventResponse
import Test.Hspec

spec :: Spec
spec = describe "HelloEventResponse" $ do
  describe "FromJson" $ do
    describe "parseJSON" $ do
      describe "if op equal 10" $ do
        it "is return to Just HelloEventResponse" $ do
          let json = "{\"op\": 10}"
          Data.Aeson.decode @HelloEventResponse json `shouldBe` Just HelloEventResponse
      describe "if op equal 11" $ do
        it "is return to Just HelloEventResponse" $ do
          let json = "{\"op\": 11}"
          Data.Aeson.decode @HelloEventResponse json `shouldBe` Nothing
