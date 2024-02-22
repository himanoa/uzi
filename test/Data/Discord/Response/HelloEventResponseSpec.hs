{-# LANGUAGE OverloadedStrings #-}
module Data.Discord.Response.HelloEventResponseSpec (
  spec
) where
import Test.Hspec
import Data.Aeson (decode)
import Data.Discord.Response.HelloEventResponse

spec::Spec
spec = describe "HelloEventResponse" $ do 
  describe "FromJson" $ do
    describe "parseJSON" $ do
      it "is return to Just HelloEventResponse" $ do
        let json = "{\"op\": 10}"
        Data.Aeson.decode @HelloEventResponse json `shouldBe` Just HelloEventResponse

