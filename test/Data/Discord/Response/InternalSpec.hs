{-# LANGUAGE OverloadedStrings #-}

module Data.Discord.Response.InternalSpec (
  spec
) where

import Test.Hspec
import Data.Discord.Response
import Data.Aeson (decode)

spec::Spec
spec = describe "Response" $ do
  describe "FromJson" $ do
    describe "parseJSON" $ do
      describe "if receive HelloEventResponse" $ do
        it "is return to Hello" $ do
          let json = "{\"op\": 10}"
          decode @Response json `shouldBe` (Just . Hello $ HelloEventResponse)
        
