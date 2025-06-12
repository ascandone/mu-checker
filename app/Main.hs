{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import qualified CCS.Parser
import qualified CCS.Program
import qualified Control.Monad
import qualified Data.Text.IO
import Mu.Verify (FailingSpec (FalsifiedFormula))
import qualified Mu.Verify
import System.Environment (getArgs)

main :: IO ()
main = do
  [filePath] <- getArgs
  content <- Data.Text.IO.readFile filePath
  case CCS.Parser.parse filePath content of
    Left e -> putStrLn $ CCS.Parser.errorBundlePretty e
    Right parsed -> do
      Control.Monad.forM_ (Mu.Verify.verifyProgram parsed) $
        \(def, e) -> case e of
          Left err -> print err
          Right failingsSpecs ->
            Control.Monad.forM_ failingsSpecs $ \(FalsifiedFormula formula) ->
              print (def.name, formula)
