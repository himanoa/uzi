{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Effectful.Req
  ( request,
    Request,
    runRequest,
  )
where

import Data.Proxy
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Internal.Monad (unsafeEff_)
import Network.HTTP.Req qualified as R

data Request :: Effect where
  Req ::
    ( R.HttpMethod method,
      R.HttpBody body,
      R.HttpResponse response,
      R.HttpBodyAllowed (R.AllowsBody method) (R.ProvidesBody body)
    ) =>
    method ->
    R.Url scheme ->
    body ->
    Proxy response ->
    R.Option scheme ->
    Request m response

type instance DispatchOf Request = Dynamic

request ::
  ( Request :> es,
    R.HttpMethod method,
    R.HttpBody body,
    R.HttpResponse response,
    R.HttpBodyAllowed (R.AllowsBody method) (R.ProvidesBody body)
  ) =>
  method ->
  R.Url scheme ->
  body ->
  Proxy response ->
  R.Option scheme ->
  Eff es response
request method url body response scheme = send (Req method url body response scheme)

runRequest :: Eff (Request : es) a -> Eff es a
runRequest = interpret $ \_ -> \case
  Req method url body r o -> unsafeEff_ . R.runReq R.defaultHttpConfig $ do
    R.req method url body r o
