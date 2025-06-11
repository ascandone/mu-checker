module Main where

import qualified CCS.Program.Parser

import qualified CCS.Program.Checker
import qualified Control.Monad
import qualified Data.Text.IO
import System.Environment (getArgs)

main :: IO ()
main = do
  [filePath] <- getArgs
  content <- Data.Text.IO.readFile filePath
  case CCS.Program.Parser.parse filePath content of
    Left e -> putStrLn $ CCS.Program.Parser.errorBundlePretty e
    Right parsed -> do
      Control.Monad.forM_ (CCS.Program.Checker.verifyProgram parsed) $ \failing -> do
        print failing
      putStrLn "done"
