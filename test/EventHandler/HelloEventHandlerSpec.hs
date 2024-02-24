{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE IncoherentInstances #-}

module EventHandler.HelloEventHandlerSpec (
  spec
) where
import Test.Hspec
import EnvConfig
import EventHandler.HelloEventHandler (helloEventHandler)
import Data.Discord.Response
import Helper.DummyDiscordGatewayInterpreter (runDummyDiscordGateway)
import Effectful.State.Static.Local (runState )
import Effectful
import Data.Discord
import Data.Discord.Request.IdentifyRequest
import Effectful.DynamicLogger

spec::Spec
spec = describe "HelloEventHandler" $ do
  describe "helloEventHandler" $ do
    it "should be send identify response" $ do
      let config = (EnvConfig { discordApiToken = "xxx" })
      let helloEventResponse = Hello HelloEventResponse
      (_, request) <- pure. runPureEff $ do
        runState @(Maybe Request) Nothing . runSilentDynamicLogger $ runDummyDiscordGateway (helloEventHandler config helloEventResponse)

      request `shouldBe` (Just . Identify . defaultIdentifyRequest $ "xxx")
