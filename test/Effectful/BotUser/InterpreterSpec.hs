{-# LANGUAGE BlockArguments #-}

module Effectful.BotUser.InterpreterSpec
  ( spec,
  )
where

import Data.Default
import Data.Discord.User
import Effectful
import Effectful.BotUser.Effect (getBotUser, setBotUser)
import Effectful.BotUser.Interpreter
import Effectful.State.Static.Shared (evalState, execState)
import Test.Hspec

spec :: Spec
spec = describe "BotUserInterpreter" $ do
  describe "runBotUser" $ do
    describe "getBotUser" $ do
      context "when not set user" $ do
        it "should be return to Nothing" $ do
          (runPureEff . evalState @(Maybe User) Nothing . runBotUser $ getBotUser) `shouldBe` Nothing
      context "when set user" $ do
        it "should be return to Just user" $ do
          let user = def @User
          (runPureEff . evalState @(Maybe User) (Just user) . runBotUser $ getBotUser) `shouldBe` Just user

    describe "setBotUser" $ do
      context "when not set user" $ do
        it "should be return to Just user" $ do
          let user = def @User
          actual <- pure . runPureEff . execState @(Maybe User) Nothing . runBotUser $ do
            _ <- setBotUser user
            pure ()
          actual `shouldBe` Just user
