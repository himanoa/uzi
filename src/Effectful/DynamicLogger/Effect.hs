{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Effectful.DynamicLogger.Effect where

import Effectful
import Effectful.Dispatch.Dynamic (send)
import RIO

data DynamicLogger :: Effect where
  Info :: Utf8Builder -> DynamicLogger m ()
  Attention :: Utf8Builder -> DynamicLogger m ()

type instance DispatchOf DynamicLogger = Dynamic

info :: (HasCallStack, DynamicLogger :> es) => Utf8Builder -> Eff es ()
info t = send (Info t)

attention :: (HasCallStack, DynamicLogger :> es) => Utf8Builder -> Eff es ()
attention t = send (Attention t)
