{-# LANGUAGE OverloadedStrings #-}

module MuFormulaParserTests (suite) where

import qualified Data.Text as Text
import Mu.Formula (Formula (..))
import qualified Mu.Formula.Parser as P
import Test.Tasty (testGroup)
import qualified Test.Tasty as Tasty
import Test.Tasty.HUnit ((@?=))
import qualified Test.Tasty.HUnit

testCase :: String -> Formula -> Tasty.TestTree
testCase src expected =
  Test.Tasty.HUnit.testCase
    src
    (P.parse (Text.pack src) @?= Right expected)

tests :: [Tasty.TestTree]
tests =
  [ testCase "x" "x"
  , testCase "!x" $
      Not "x"
  , testCase "!! x" $
      Not (Not "x")
  , testCase "!a && !b" $
      Not "a" `And` Not "b"
  ]

suite :: Tasty.TestTree
suite = testGroup "MuFormulaParser" tests
