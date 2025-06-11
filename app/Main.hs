{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import CCS.Checker (FailingSpec (FalsifiedFormula))
import qualified CCS.Checker
import qualified CCS.Parser
import qualified CCS.Program
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
      Control.Monad.forM_ (CCS.Checker.verifyProgram parsed) $
        \(def, e) -> case e of
          Left err -> print err
          Right failingsSpecs ->
            Control.Monad.forM_ failingsSpecs $ \(FalsifiedFormula formula) ->
              print (def.name, formula)
