{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
module Effectful.DiscordApiTokenReader.Effect  where

import Effectful
import Effectful.Dispatch.Dynamic( send )
import Data.Text

data DiscordApiTokenReader :: Effect where
  GetToken :: DiscordApiTokenReader m Text

type instance DispatchOf DiscordApiTokenReader = Dynamic

getToken :: (DiscordApiTokenReader :> es) => Eff es Text 
getToken = send GetToken
