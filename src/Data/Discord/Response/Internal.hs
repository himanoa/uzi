module Data.Discord.Response.Internal (
  Response(..)
) where

import Data.Discord.Response.HelloEventResponse (HelloEventResponse (HelloEventResponse))
import Data.Aeson

data Response = Hello HelloEventResponse
  deriving (Show, Eq)

instance FromJSON Response  where
  parseJSON v = do
    payload <- parseJSON @HelloEventResponse v
    case payload of
      HelloEventResponse-> pure . Hello $ HelloEventResponse
      -- _ -> prependFailure "Not supported event" ( typeMismatch "Response" v )
