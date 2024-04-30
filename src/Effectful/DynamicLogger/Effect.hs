{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- |
-- Module: Effectful.DynamicLogger.Effect
-- Description: ログを出力するためのEffectです
-- Maintainer: himanoa <matsunoappy@gmail.com>
--
-- Logを出力するためのEffectです。
module Effectful.DynamicLogger.Effect where

import Effectful
import Effectful.Dispatch.Dynamic (send)
import RIO

data DynamicLogger :: Effect where
  Info :: Utf8Builder -> DynamicLogger m ()
  Attention :: Utf8Builder -> DynamicLogger m ()

type instance DispatchOf DynamicLogger = Dynamic

-- | ログを出力します
info ::
  (HasCallStack, DynamicLogger :> es) =>
  -- | ログに出力する内容
  Utf8Builder ->
  Eff es ()
info t = send (Info t)

-- | エラーログを出力します
attention ::
  (HasCallStack, DynamicLogger :> es) =>
  -- | ログに出力する内容
  Utf8Builder ->
  Eff es ()
attention t = send (Attention t)
