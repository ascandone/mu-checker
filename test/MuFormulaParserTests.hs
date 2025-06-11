{-# LANGUAGE OverloadedStrings #-}

module MuFormulaParserTests (suite) where

import qualified Data.Text as Text
import Mu.Formula (Formula (..), FormulaEvent (..), box, evtAlways, evtOr)
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
  , testCase "<false> x" $
      Diamond EvtBottom "x"
  , testCase "[false] x" $
      box EvtBottom "x"
  , testCase "<false> <false> x" $
      Diamond EvtBottom (Diamond EvtBottom "x")
  , testCase "<false || !true> x" $
      Diamond (EvtBottom `evtOr` EvtNot evtAlways) "x"
  , testCase "<a?> x" $
      Diamond (Rcv "a") "x"
  , testCase "<a!> x" $
      Diamond (Snd "a") "x"
  , testCase "<a!> <b?> x" $
      Diamond (Snd "a") $
        Diamond (Rcv "b") "x"
  , testCase "<tau> x" $
      Diamond Tau "x"
  ]

suite :: Tasty.TestTree
suite = testGroup "MuFormulaParser" tests
