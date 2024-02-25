{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Effectful.Req (
  request,
  Request
) where
import Effectful
import Effectful.Dispatch.Static
import Network.HTTP.Req
import Data.Proxy

data Request :: Effect

type instance DispatchOf Request = Static WithSideEffects

request :: (
  IOE :> es,
  HttpMethod method,
  HttpBody body,
  HttpResponse response,
  HttpBodyAllowed (AllowsBody method) (ProvidesBody body)
  ) =>method -> Url scheme -> body -> Proxy response -> Eff es response
request method url body response = do
  r <- runReq defaultHttpConfig $ 
    req method url body response mempty
  unsafeEff_ $ pure r
