{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Effectful.DynamicLogger.Effect where

import Data.Text
import Effectful
import Effectful.Dispatch.Dynamic (HasCallStack, send)

data DynamicLogger :: Effect where
  Info :: Text -> DynamicLogger m ()
  Attention :: Text -> DynamicLogger m ()


type instance DispatchOf DynamicLogger = Dynamic

info :: (HasCallStack, DynamicLogger :> es) => Text -> Eff es ()
info t = send (Info t)

attention :: (HasCallStack, DynamicLogger :> es) => Text -> Eff es ()
attention t = send (Attention t)
