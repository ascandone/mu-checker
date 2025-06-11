module Main where

import qualified CCS.Program.Parser

import qualified CCS.Program.Checker
import qualified Control.Monad
import qualified Data.Text.IO
import System.Environment (getArgs)

main :: IO ()
main = do
  [arg] <- getArgs
  content <- Data.Text.IO.readFile arg
  case CCS.Program.Parser.parse content of
    Left e -> print e
    Right parsed -> do
      Control.Monad.forM_ (CCS.Program.Checker.verifyProgram parsed) $ \failing -> do
        print failing
      putStrLn "done"
