{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Effectful.BotUser.Effect where

import Data.Discord.User
import Effectful
import Effectful.Dispatch.Dynamic

data BotUser :: Effect where
  SetBotUser :: User -> BotUser m ()
  GetBotUser :: BotUser m (Maybe User)

type instance DispatchOf BotUser = Dynamic

setBotUser :: (HasCallStack, BotUser :> es) => User -> Eff es ()
setBotUser = send . SetBotUser

getBotUser :: (HasCallStack, BotUser :> es) => Eff es (Maybe User)
getBotUser = send GetBotUser
