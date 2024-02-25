{-# LANGUAGE OverloadedStrings #-}

module Data.Discord.Response.InternalSpec
  ( spec,
  )
where

import Data.Aeson (decode)
import Data.Discord.Response
import Test.Hspec
import Data.Discord.Response.ReadyEventResponse

spec :: Spec
spec = describe "Response" $ do
  describe "FromJson" $ do
    describe "parseJSON" $ do
      describe "if receive HelloEventResponse" $ do
        it "is return to Hello" $ do
          let json = "{\"op\": 10}"
          decode @Response json `shouldBe` (Just . Hello $ HelloEventResponse)
      describe "if receive HelloEventResponse" $ do
        it "is return to Ready" $ do
          let json = "{\"op\": 0, \"t\": \"READY\"}"
          decode @Response json `shouldBe` (Just . Ready $ ReadyEventResponse)
