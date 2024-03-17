{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Effectful.DiscordApiTokenReader.Effect where

import Data.Text
import Effectful
import Effectful.Dispatch.Dynamic (send)

data DiscordApiTokenReader :: Effect where
  GetToken :: DiscordApiTokenReader m Text

type instance DispatchOf DiscordApiTokenReader = Dynamic

getToken :: (DiscordApiTokenReader :> es) => Eff es Text
getToken = send GetToken
