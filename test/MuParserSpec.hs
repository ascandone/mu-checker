{-# LANGUAGE OverloadedStrings #-}

module MuParserSpec (spec) where

import qualified Data.Text as Text
import Mu.Formula (Evt (..), Formula (..), FormulaEvent (..), box, evtBottom, evtOr, lor)
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

  testCase "<true> x" $
    Diamond Up "x"

  testCase "[true] x" $
    box Up "x"

  testCase "<true> <true> x" $
    Diamond Up (Diamond Up "x")

  testCase "<true || !false> x" $
    Diamond (Up `evtOr` EvtNot evtBottom) "x"

  testCase "<a?> x" $
    Diamond (rcv "a") "x"

  testCase "<a!> x" $
    Diamond (snd "a") "x"

  testCase "<a!> <b?> x" $
    Diamond (snd "a") $
      Diamond (rcv "b") "x"

  testCase "<tau> x" $
    Diamond tau "x"

  testCase "mu x. y" $
    Mu "x" "y"

  testCase "mu x. mu y . z" $
    Mu "x" $
      Mu "y" "z"

  testCase "mu x. a && b" $
    Mu "x" $
      "a" `And` "b"

  testCase "! mu a . b" $
    Not $
      Mu "a" "b"

  testCase "! (mu x.[true] false || !x)" $
    Not $
      Mu "x" $
        lor (box Up Bottom) (Not "x")

  -- TODO fix prec
  testCase "! mu x. [true] false || !x" $
    unwrapRight $
      P.parse "(! mu x. [true] false) || !x"

unwrapRight :: (Show a) => Either a b -> b
unwrapRight e = case e of
  Right x -> x
  Left err -> error (show err)
