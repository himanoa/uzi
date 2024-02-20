module Main (main) where

import Control.Exception.Safe
import Lib
import System.Exit

main :: IO ()
main =
  runUzi `catch` \(e :: UziError) -> do
    case e of
      CrashError -> exitWith (ExitFailure 1)
