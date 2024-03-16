{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Data.Discord.Content
  ( Content,
    makeContent,
    makeUnsafeContent,
  )
where

import Data.Aeson hiding (Success)
import Data.Either.Validation
import Data.Text
import Prelude hiding (length)

newtype Content = Content Text
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via Text

data MakeContentError = OverMaxContentLength Text
  deriving (Show, Eq)

makeContent :: Text -> Validation MakeContentError Content
makeContent text =
  if length text > 2000
    then Failure . OverMaxContentLength $ text
    else Success . Content $ text

makeUnsafeContent :: Text -> Content
makeUnsafeContent = Content
