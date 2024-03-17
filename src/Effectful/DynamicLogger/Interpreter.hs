{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Effectful.DynamicLogger.Interpreter where

import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.DynamicLogger.Effect
import Effectful.Log.Static

runDynamicLogger :: (Log :> es) => Eff (DynamicLogger : es) a -> Eff es a
runDynamicLogger = interpret $ \_ -> \case
  Info t -> logInfo t
  Attention t -> logError t

runSilentDynamicLogger :: Eff (DynamicLogger : es) a -> Eff es a
runSilentDynamicLogger = interpret $ \_ -> \case
  Info _ -> pure ()
  Attention _ -> pure ()
