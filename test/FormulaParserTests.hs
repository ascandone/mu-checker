{-# LANGUAGE OverloadedStrings #-}

module FormulaParserTests (suite) where

import qualified Data.Text as Text
import LTL.Formula (Formula (..), globally)
import qualified LTL.Parser as P
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
  , testCase "X X x" $
      Next (Next "x")
  , testCase "X !x" $
      Next (Not "x")
  , testCase " ! (X x)" $
      Not (Next "x")
  , testCase " ! ! X x" $
      Not (Not (Next "x"))
  , testCase "!a && !b" $
      Not "a" `And` Not "b"
  , testCase "G X x" $
      globally (Next "x")
  , testCase "a U b" $
      "a" `Until` "b"
  ]

suite :: Tasty.TestTree
suite = testGroup "LTLFormulaTests" tests
