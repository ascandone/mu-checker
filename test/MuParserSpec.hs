{-# LANGUAGE OverloadedStrings #-}

module MuParserSpec (spec) where

import qualified Data.Text as Text
import Mu.Formula (Evt (..), Formula (..), FormulaEvent (..), box, evtAlways, evtOr)
import qualified Mu.Parser as P
import Test.Hspec
import Prelude hiding (snd)

testCase :: String -> Formula -> SpecWith ()
testCase src expected =
  it src (P.parse (Text.pack src) `shouldBe` Right expected)

rcv :: Text.Text -> FormulaEvent
rcv = Evt . Rcv

snd :: Text.Text -> FormulaEvent
snd = Evt . Snd

tau :: FormulaEvent
tau = Evt Tau

spec :: Spec
spec = do
  testCase "x" "x"
  testCase "!x" $
    Not "x"
  testCase "!! x" $
    Not (Not "x")
  testCase "!a && !b" $
    Not "a" `And` Not "b"
  testCase "<false> x" $
    Diamond EvtBottom "x"
  testCase "[false] x" $
    box EvtBottom "x"
  testCase "<false> <false> x" $
    Diamond EvtBottom (Diamond EvtBottom "x")
  testCase "<false || !true> x" $
    Diamond (EvtBottom `evtOr` EvtNot evtAlways) "x"
  testCase "<a?> x" $
    Diamond (rcv "a") "x"
  testCase "<a!> x" $
    Diamond (snd "a") "x"
  testCase "<a!> <b?> x" $
    Diamond (snd "a") $
      Diamond (rcv "b") "x"
  testCase "<tau> x" $
    Diamond tau "x"
