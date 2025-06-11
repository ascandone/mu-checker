{-# LANGUAGE OverloadedStrings #-}

module MuFormulaParserTests (suite) where

import Mu.Formula (Formula (..))
import qualified Mu.Formula.Parser as P
import Test.Tasty (testGroup)
import qualified Test.Tasty as Tasty
import Test.Tasty.HUnit (testCase, (@?=))

tests :: [Tasty.TestTree]
tests =
  [ testCase "atom" $
      P.parse "x" @?= Right "x"
  , testCase "prefix !" $
      P.parse "!x" @?= Right (Not "x")
  , testCase "double !" $
      P.parse "!!x" @?= Right (Not (Not "x"))
  , testCase "and, ! prec" $
      P.parse "!x && !y" @?= Right (Not "x" `And` Not "y")
  ]

suite :: Tasty.TestTree
suite = testGroup "MuFormulaParser" tests
