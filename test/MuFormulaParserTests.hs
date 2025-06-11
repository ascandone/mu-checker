{-# LANGUAGE OverloadedStrings #-}

module MuFormulaParserTests (suite) where

import qualified Data.Text as Text
import Mu.Formula (Evt (..), Formula (..), FormulaEvent (..), box, evtAlways, evtOr)
import qualified Mu.Formula.Parser as P
import Test.Tasty (testGroup)
import qualified Test.Tasty as Tasty
import Test.Tasty.HUnit ((@?=))
import qualified Test.Tasty.HUnit
import Prelude hiding (snd)

testCase :: String -> Formula -> Tasty.TestTree
testCase src expected =
  Test.Tasty.HUnit.testCase
    src
    (P.parse (Text.pack src) @?= Right expected)

rcv :: Text.Text -> FormulaEvent
rcv = Evt . Rcv
snd :: Text.Text -> FormulaEvent
snd = Evt . Snd

tau :: FormulaEvent
tau = Evt Tau

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
      Diamond (rcv "a") "x"
  , testCase "<a!> x" $
      Diamond (snd "a") "x"
  , testCase "<a!> <b?> x" $
      Diamond (snd "a") $
        Diamond (rcv "b") "x"
  , testCase "<tau> x" $
      Diamond tau "x"
  ]

suite :: Tasty.TestTree
suite = testGroup "MuFormulaParser" tests
