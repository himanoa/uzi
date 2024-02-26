{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Effectful.DiscordApiTokenReader.Effect where

import Data.Text
import Effectful
import Effectful.Dispatch.Dynamic (send)

data DiscordApiTokenReader :: Effect where
  GetToken :: DiscordApiTokenReader m Text

type instance DispatchOf DiscordApiTokenReader = Dynamic

getToken :: (DiscordApiTokenReader :> es) => Eff es Text
getToken = send GetToken
