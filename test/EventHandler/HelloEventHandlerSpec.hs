{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module EventHandler.HelloEventHandlerSpec
  ( spec,
  )
where

import Data.Discord
import Data.Discord.Request.IdentifyRequest
import Data.Discord.Response.HelloEventResponse
import Effectful
import Effectful.DiscordApiTokenReader
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.DynamicLogger
import Effectful.NonDet
import Effectful.State.Static.Local (runState)
import EventHandler.HelloEventHandler (helloEventHandler)
import Helper.DummyDiscordGatewayInterpreter (runDummyDiscordGateway)
import Test.Hspec

runDummyDiscordApiTokenReader :: Eff (DiscordApiTokenReader : es) a -> Eff es a
runDummyDiscordApiTokenReader = interpret $ \_ -> \case
  GetToken -> pure "xxx"

spec :: Spec
spec = describe "HelloEventHandler" $ do
  describe "helloEventHandler" $ do
    it "should be send identify response" $ do
      let helloEventResponse = Hello HelloEventResponse
      (_, request) <- pure . runPureEff $ do
        runState @(Maybe Request) Nothing . runNonDet OnEmptyKeep . runSilentDynamicLogger . runDummyDiscordGateway $ runDummyDiscordApiTokenReader (helloEventHandler helloEventResponse)

      request `shouldBe` (Just . Identify . defaultIdentifyRequest $ "xxx")
