{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Effectful.DynamicLogger.Interpreter where

-- \|
-- Module: Effectful.DynamicLogger.Effect
-- Description: ログを出力するためのEffectです
-- Maintainer: himanoa <matsunoappy@gmail.com>
--
-- Logを出力するためのEffectです。

import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.DynamicLogger.Effect
import Effectful.Log.Static
import RIO hiding (logError, logInfo)

-- | 本番の環境で実行するロガーです。RIO.Loggerに合わせて出力結果が選択されます
runDynamicLogger :: (Log :> es) => Eff (DynamicLogger : es) a -> Eff es a
runDynamicLogger = interpret $ \_ -> \case
  Info t -> logInfo t
  Attention t -> logError t

-- | テスト環境で実行されるロガーです。何も表示しません
runSilentDynamicLogger :: Eff (DynamicLogger : es) a -> Eff es a
runSilentDynamicLogger = interpret $ \_ -> \case
  Info _ -> pure ()
  Attention _ -> pure ()
