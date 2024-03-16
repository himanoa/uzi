{-# LANGUAGE BlockArguments #-}
module Effectful.BotUser.InterpreterSpec (
  spec
) where

import Test.Hspec
import Effectful.State.Static.Local (evalState,  execState)
import Effectful.BotUser.Effect (getBotUser, setBotUser)
import Effectful.BotUser.Interpreter
import Effectful
import Data.Default
import Data.Discord.User

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
          actual  `shouldBe`  Just user




