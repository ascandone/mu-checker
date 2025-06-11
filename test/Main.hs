module Main (main) where

import qualified CCSLTSTests
import qualified CCSProgramParserTests
import qualified FormulaParserTests
import qualified LTLCheckerTests
import qualified MuFormulaParserTests

import Test.Hspec (hspec)

main :: IO ()
main =
  hspec $ do
    CCSLTSTests.suite
    CCSProgramParserTests.suite
    FormulaParserTests.suite
    LTLCheckerTests.suite
    MuFormulaParserTests.suite
