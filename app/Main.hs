module Main (main) where

import Lib
import Control.Exception.Safe
import System.Exit

main :: IO ()

main = runUzi `catch` \(e :: UziError) -> do
  case e of 
    CrashError -> exitWith (ExitFailure 1)
