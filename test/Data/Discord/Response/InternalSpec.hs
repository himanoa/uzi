{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DisambiguateRecordFields #-}

module Data.Discord.Response.InternalSpec
  ( spec,
  )
where

import Data.Aeson (decode)
import Data.Discord
import Data.Discord.Response.HelloEventResponse
import Data.Discord.Response.MessageCreateEventResponse
import Data.Discord.Response.ReadyEventResponse
import Test.Hspec
import Data.Discord.Content 

spec :: Spec
spec = describe "Response" $ do
  describe "FromJson" $ do
    describe "parseJSON" $ do
      describe "if receive HelloEventResponse" $ do
        it "is return to Hello" $ do
          let json = "{\"op\": 10}"
          decode @Response json `shouldBe` (Just . Hello $ HelloEventResponse)
      describe "if receive ReadyEventResponse" $ do
        it "is return to Ready" $ do
          let json = "{\"op\": 0, \"t\": \"READY\"}"
          decode @Response json `shouldBe` (Just . Ready $ ReadyEventResponse)
      describe "if receive MessageCreateEventResponse" $ do
        it "is return to Ready" $ do
          let j = "{\"t\":\"MESSAGE_CREATE\",\"s\":3,\"op\":0,\"d\":{\"type\":0,\"tts\":false,\"timestamp\":\"2024-02-25T05:21:07.222000+00:00\",\"referenced_message\":null,\"pinned\":false,\"nonce\":\"1211181057097859072\",\"mentions\":[],\"mention_roles\":[],\"mention_everyone\":false,\"member\":{\"roles\":[\"576775513373278208\",\"872812731688964106\",\"854323955870990346\",\"872811979201458186\",\"595972214608625676\",\"978548133065809931\",\"1039098706466709514\",\"872811726708568146\",\"774210208741195797\",\"874440465439682650\",\"872812820100694077\",\"872811935832346654\"],\"premium_since\":\"2019-07-03T15:11:41.990000+00:00\",\"pending\":false,\"nick\":\"himanoa\",\"mute\":false,\"joined_at\":\"2019-05-11T05:55:50.572000+00:00\",\"flags\":0,\"deaf\":false,\"communication_disabled_until\":null,\"avatar\":null},\"id\":\"1211181059421765652\",\"flags\":0,\"embeds\":[],\"edited_timestamp\":null,\"content\":\"\",\"components\":[],\"channel_id\":\"576775857839013888\",\"author\":{\"username\":\"himanoa\",\"public_flags\":576,\"premium_type\":2,\"id\":\"202022053790875648\",\"global_name\":\"himanoa\",\"discriminator\":\"0\",\"avatar_decoration_data\":{\"sku_id\":\"1144048390594908212\",\"asset\":\"a_db9baf0ba7cf449d2b027c06309dbe8d\"},\"avatar\":\"ea9b0cf42bb8013620cd5943ad7fe580\"},\"attachments\":[],\"guild_id\":\"576648644942495744\"}}"
          decode @MessageCreateEventResponse j
            `shouldBe` Just
              MessageCreateEventResponse
                { channelId = ChannelId "576775857839013888",
                  content =  makeUnsafeContent "",
                  mentions = [],
                  member =
                    Member
                      { roles =
                          map
                            Role
                            [ "576775513373278208",
                              "872812731688964106",
                              "854323955870990346",
                              "872811979201458186",
                              "595972214608625676",
                              "978548133065809931",
                              "1039098706466709514",
                              "872811726708568146",
                              "774210208741195797",
                              "874440465439682650",
                              "872812820100694077",
                              "872811935832346654"
                            ],
                        nick = Just . Nickname $ "himanoa"
                      },
                  isBot = False
                }
