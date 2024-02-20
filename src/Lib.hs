{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( runUzi
    ) where
import Effectful (runEff)
import Effectful.Log
import Log.Backend.StandardOutput

runUzi :: IO ()

runUzi = runEff $ do
  withStdOutLogger $ \stdoutLogger -> do
    runLog "Uzi" stdoutLogger defaultLogLevel $ do 
      logInfo_ "Hello uzi"
