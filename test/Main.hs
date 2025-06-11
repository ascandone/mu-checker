module Main (main) where

import qualified CCSLTSTests
import qualified CCSProgramParserTests
import qualified FormulaParserTests
import qualified LTLCheckerTests
import qualified MuFormulaParserTests
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ LTLCheckerTests.suite
    , FormulaParserTests.suite
    , MuFormulaParserTests.suite
    , CCSProgramParserTests.suite
    , CCSLTSTests.suite
    ]
