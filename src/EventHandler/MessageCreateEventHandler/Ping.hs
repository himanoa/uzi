{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module EventHandler.MessageCreateEventHandler.Ping  where
import Effectful.DiscordGateway 
import Effectful.DynamicLogger 
import Data.Discord 
import EnvConfig
import Effectful

pingEventHandler :: (DiscordGateway :> es, DynamicLogger :> es) => EnvConfig -> Response -> Eff es ()
pingEventHandler _ = \case
  MessageCreate x -> info "Received Message Created"
  _ -> pure()
