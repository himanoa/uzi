{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Data.Discord.Content
  ( Content,
    makeContent,
    makeUnsafeContent,
    body
  )
where

import Data.Aeson hiding (Success)
import Data.Either.Validation
import Data.Text hiding (concat)
import Text.Parsec qualified as P
import Prelude hiding (length)
import qualified Text.Parsec.Text as P
import Data.Coerce
import Data.Either.Combinators (rightToMaybe)

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

body :: Content -> Maybe Text
body c = do
  let contentText = id @Text . coerce $ c
  let contentMaybe =  rightToMaybe (P.runParser parser () "dummy" contentText)
  fmap (strip . pack . concat) contentMaybe

  where
    userIdSymParser :: P.Parser String
    userIdSymParser = P.between (P.char '<') (P.char '>') (P.char '@' >> P.many1 P.digit)

    contentParser :: P.Parser String
    contentParser =  P.many1 . P.noneOf $ "<"

    fallbackParser :: P.Parser String
    fallbackParser =  P.many1 P.anyChar

    parser :: P.Parser [String] 
    parser = P.many1 $ (P.try userIdSymParser >> return "" ) P.<|> contentParser P.<|> fallbackParser

