module Main (main) where

import qualified CheckerTests
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ CheckerTests.suite
    ]
