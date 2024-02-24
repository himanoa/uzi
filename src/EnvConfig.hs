module EnvConfig where

import Data.Text

data EnvConfig = EnvConfig
  {discordApiToken :: Text}
  deriving (Show)
