module OpcodeSpec
  ( spec,
  )
where

import Lib (OperationCode (ClientConnect, ClientDisconnect, Heartbeat, HeartbeatAck, Hello, Identify, Ready, Resume, Resumed, SelectProtocol, SessionDescription, Speaking))
import Test.Hspec

spec :: Spec
spec = describe "Opcode" $ do
  describe "fromEnum" $ do
    describe "input is Identify" $ do
      it "is return to 0" $ do
        fromEnum Identify `shouldBe` 0

    describe "input is SelectProtocol" $ do
      it "is return to 1" $ do
        fromEnum SelectProtocol `shouldBe` 1

    describe "input is Ready" $ do
      it "is return to 2" $ do
        fromEnum Ready `shouldBe` 2

    describe "input is Heartbeat" $ do
      it "is return to 3" $ do
        fromEnum Heartbeat `shouldBe` 3

    describe "input is SessionDescription" $ do
      it "is return to 4" $ do
        fromEnum SessionDescription `shouldBe` 4

    describe "input is Speaking" $ do
      it "is return to 5" $ do
        fromEnum Speaking `shouldBe` 5

    describe "input is HeartbeatAck" $ do
      it "is return to 6" $ do
        fromEnum HeartbeatAck `shouldBe` 6

    describe "input is Resume" $ do
      it "is return to 7" $ do
        fromEnum Resume `shouldBe` 7

    describe "input is Hello" $ do
      it "is return to 8" $ do
        fromEnum Hello `shouldBe` 8

    describe "input is Resumed" $ do
      it "is return to 9" $ do
        fromEnum Resumed `shouldBe` 9

    describe "input is ClientConnect" $ do
      it "is return to 10" $ do
        fromEnum ClientConnect `shouldBe` 10

    describe "input is ClientDisconnect" $ do
      it "is return to 11" $ do
        fromEnum ClientDisconnect `shouldBe` 11
