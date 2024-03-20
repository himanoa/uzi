VERSION 0.8
ARG BASE_TAG="himanoa/uzi:"

IMPORT github.com/lugendre/earthly-haskell/base/alpine
IMPORT github.com/lugendre/earthly-haskell/haskell
IMPORT github.com/lugendre/earthly-haskell/installer

build:
  FROM alpine+alpine3.19.1
  DO installer+SETUP_HASKELL --GHC=9.6.4 --HPACK_VERSION=0.36.0
  # Cache the base image.
  SAVE IMAGE --push "${BASE_TAG}alpine3.19.1-ghc9.6.4"
  WORKDIR /production
  DO haskell+INIT
  COPY --keep-ts . .
  # Generate *.cabal files from package.yaml.
  DO haskell+CABAL --args="update"
  DO haskell+PRODUCTION_BUILD_STATIC \
    --target="all" \
    --output="output" \
    --extra-args="--jobs --constraint=\"vector -boundschecks\""
  SAVE ARTIFACT output AS LOCAL output
  SAVE IMAGE --push "${BASE_TAG}uzi-exe"
