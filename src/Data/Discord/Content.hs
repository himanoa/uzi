{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Data.Discord.Content
  ( Content,
    makeContent,
    makeUnsafeContent,
    body,
  )
where

import Data.Aeson hiding (Success)
import Data.Coerce
import Data.Either.Combinators (rightToMaybe)
import Data.Either.Validation
import RIO.Text qualified as T
import Text.Parsec qualified as P
import Text.Parsec.Text qualified as P
import Prelude hiding (length)

newtype Content = Content T.Text
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via T.Text

data MakeContentError = OverMaxContentLength T.Text
  deriving (Show, Eq)

makeContent :: T.Text -> Validation MakeContentError Content
makeContent text =
  if T.length text > 2000
    then Failure . OverMaxContentLength $ text
    else Success . Content $ text

makeUnsafeContent :: T.Text -> Content
makeUnsafeContent = Content

body :: Content -> Maybe T.Text
body c = do
  let contentText = id @T.Text . coerce $ c
  let contentMaybe = rightToMaybe (P.runParser parser () "dummy" contentText)
  fmap (T.strip . T.pack . concat) contentMaybe
  where
    userIdSymParser :: P.Parser String
    userIdSymParser = P.between (P.char '<') (P.char '>') (P.char '@' >> P.many1 P.digit)

    contentParser :: P.Parser String
    contentParser = P.many1 . P.noneOf $ "<"

    fallbackParser :: P.Parser String
    fallbackParser = P.many1 P.anyChar

    parser :: P.Parser [String]
    parser = P.many1 $ (P.try userIdSymParser >> return "") P.<|> contentParser P.<|> fallbackParser
