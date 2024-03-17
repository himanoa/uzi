{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Effectful.DynamicLogger.Effect where

import RIO
import Effectful
import Effectful.Dispatch.Dynamic (send)

data DynamicLogger :: Effect where
  Info :: Utf8Builder -> DynamicLogger m ()
  Attention :: Utf8Builder -> DynamicLogger m ()

type instance DispatchOf DynamicLogger = Dynamic

info :: (HasCallStack, DynamicLogger :> es) => Utf8Builder -> Eff es ()
info t = send (Info t)

attention :: (HasCallStack, DynamicLogger :> es) => Utf8Builder -> Eff es ()
attention t = send (Attention t)
