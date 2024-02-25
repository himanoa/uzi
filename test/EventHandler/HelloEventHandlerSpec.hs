{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module EventHandler.HelloEventHandlerSpec
  ( spec,
  )
where

import Data.Discord
import Data.Discord.Request.IdentifyRequest
import Data.Discord.Response.HelloEventResponse
import Effectful
import Effectful.DynamicLogger
import Effectful.State.Static.Local (runState)
import EnvConfig
import EventHandler.HelloEventHandler (helloEventHandler)
import Helper.DummyDiscordGatewayInterpreter (runDummyDiscordGateway)
import Test.Hspec

spec :: Spec
spec = describe "HelloEventHandler" $ do
  describe "helloEventHandler" $ do
    it "should be send identify response" $ do
      let config = (EnvConfig {discordApiToken = "xxx"})
      let helloEventResponse = Hello HelloEventResponse
      (_, request) <- pure . runPureEff $ do
        runState @(Maybe Request) Nothing . runSilentDynamicLogger $ runDummyDiscordGateway (helloEventHandler config helloEventResponse)

      request `shouldBe` (Just . Identify . defaultIdentifyRequest $ "xxx")
