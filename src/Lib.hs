module Lib
    ( runUzi
    ) where
import Effectful (runEff)
import qualified Control.Applicative as Eff
import qualified Control.Monad.IO.Class as Eff

runUzi :: IO ()

runUzi = runEff $ do
  _ <- Eff.liftIO $ putStrLn "someFunc"
  Eff.pure()
