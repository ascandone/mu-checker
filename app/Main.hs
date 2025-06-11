module Main where

import qualified CCS.Checker
import qualified CCS.Parser
import qualified Control.Monad
import qualified Data.Text.IO
import System.Environment (getArgs)

main :: IO ()
main = do
  [filePath] <- getArgs
  content <- Data.Text.IO.readFile filePath
  case CCS.Parser.parse filePath content of
    Left e -> putStrLn $ CCS.Parser.errorBundlePretty e
    Right parsed -> do
      Control.Monad.forM_ (CCS.Checker.verifyProgram parsed) $ \failing -> do
        print failing
      putStrLn "done"
