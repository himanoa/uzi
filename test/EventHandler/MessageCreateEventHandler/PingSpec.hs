module EventHandler.MessageCreateEventHandler.PingSpec (
spec
) where
import Test.Hspec
import EventHandler.MessageCreateEventHandler.Ping (pingEventHandler)
import Helper.DummyDiscordChannelInterpreter
import Effectful.NonDet
import Effectful.State.Static.Local
import Effectful
import Data.Discord
import Data.Discord.Response.HelloEventResponse
import Data.Either
import Effectful.DiscordChannel.Effect

spec :: Spec
spec = describe "PingSpec" $ do
  describe "pingEventHandler" $ do
    context "when provide not MessageCreateEvent" $ do
      it "should be return to emptyEff" $ do
        let response = Hello HelloEventResponse
        let actual = runPureEff . runNonDet OnEmptyKeep . runState @(Maybe SendMessageParams ) Nothing . runDummyDiscordChannel $  pingEventHandler response
        isRight actual `shouldBe` True
