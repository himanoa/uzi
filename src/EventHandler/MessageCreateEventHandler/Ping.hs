{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module EventHandler.MessageCreateEventHandler.Ping  where
import Effectful.DiscordGateway 
import Effectful.DynamicLogger 
import Data.Discord 
import Effectful

pingEventHandler :: (DiscordGateway :> es, DynamicLogger :> es) =>  Response -> Eff es ()
pingEventHandler = \case
  MessageCreate _ -> info "Received Message Created"
  _ -> pure()
