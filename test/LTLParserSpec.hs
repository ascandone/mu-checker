{-# LANGUAGE OverloadedStrings #-}

module LTLParserSpec (spec) where

import qualified Data.Text as Text
import LTL.Formula (Formula (..), globally)
import qualified LTL.Parser as P
import Test.Hspec

testCase :: String -> Formula -> SpecWith ()
testCase src expected =
  it src $ do
    P.parse (Text.pack src) `shouldBe` Right expected

spec :: Spec
spec = do
  testCase "x" "x"

  testCase "!x" $
    Not "x"

  testCase "!! x" $
    Not (Not "x")

  testCase "X X x" $
    Next (Next "x")

  testCase "X !x" $
    Next (Not "x")

  testCase " ! (X x)" $
    Not (Next "x")

  testCase " ! ! X x" $
    Not (Not (Next "x"))

  testCase "!a && !b" $
    Not "a" `And` Not "b"

  testCase "G X x" $
    globally (Next "x")

  testCase "a U b" $
    "a" `Until` "b"
