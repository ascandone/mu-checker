module Main (main) where

import qualified CheckerTests
import qualified FormulaParserTests
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ CheckerTests.suite
    , FormulaParserTests.suite
    ]
