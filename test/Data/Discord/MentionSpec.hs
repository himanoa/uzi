{-# LANGUAGE OverloadedStrings #-}

module Data.Discord.MentionSpec
  ( spec,
  )
where

import Data.Aeson hiding (json)
import Data.Discord.Mention
import Test.Hspec

spec :: Spec
spec = describe "Mention" $ do
  describe "FromJSON" $ do
    describe "parseJSON" $ do
      it "should be return to Mention" $ do
        let json = "{\"username\": \"greatwurm\",\"public_flags\": 0,\"member\": {\"roles\": [\"1211197086343626774\"],\"premium_since\": null,\"pending\": false,\"nick\": null,\"mute\": false,\"joined_at\": \"2024-02-25T06:24:48.436238+00:00\",\"flags\": 1,\"deaf\": false,\"communication_disabled_until\": null,\"avatar\": null},\"id\": \"582274874119290880\",\"global_name\": null,\"discriminator\": \"7326\",\"bot\": true,\"avatar_decoration_data\": null,\"avatar\": \"baabc1d97a1952fc78feaadc54321b96\"}"
        decode @Mention json `shouldBe` Just (makeMention (UserId "582274874119290880") (UserName "greatwurm") Nothing True)
