{-# LANGUAGE OverloadedStrings #-}

module CCSPrettySpec (spec) where

import CCS.Parser (errorBundlePretty)
import qualified CCS.Parser
import qualified CCS.Pretty
import Data.Bifunctor (first)
import Data.Text (Text)
import Test.Hspec

spec :: Spec
spec = do
  it "prints empty proc" $ do
    shouldBePrinted "0"

  it "prints ident no args" $ do
    shouldBePrinted "X"

  it "prints ident with args" $ do
    shouldBePrinted "X(a, b)"

  -- TODO many labels sugar
  it "prints restricted ident" $ do
    shouldBePrinted "X\\a"

  it "prints unary choice" $ do
    shouldBePrinted "a?.0"

  it "prints binary choice" $ do
    shouldBePrinted "a?.0 + b!.0"

  it "prints par" $ do
    shouldBePrinted "X | Y"

  describe "labels grouping" $ do
    it "groups labels" $ do
      shouldBePrinted "X\\{a, b, c, d}"

  describe "prec" $ do
    it "doesn't need parens for nested par" $ do
      shouldBePrinted "X | Y | Z"

    it "doesn't need parens choice within par" $ do
      shouldBePrinted "x?.0 + y!.0 | Y"

    it "needs parens for par within choice" $ do
      shouldBePrinted "x?.(X | Y)"

    it "needs parens for restriction of a single choice" $ do
      shouldBePrinted "(a!.0)\\a"

    -- it "needs parens if restriction is within choice" $ do
    --   shouldBePrinted "a!.(0\\a)"

    it "needs parens for restriction of a multiple choice" $ do
      shouldBePrinted "(a!.0 + b!.0)\\a"

    it "needs parens for restriction of par" $ do
      shouldBePrinted "(X | Y)\\a"

    it "no parens if restriction is inner par" $ do
      shouldBePrinted "X\\a | Y\\a"

shouldBePrinted :: Text -> Expectation
shouldBePrinted src = do
  let parsedProc = unwrapRight $ first errorBundlePretty $ CCS.Parser.parseProc "test" src
  let prettyProc = CCS.Pretty.pretty parsedProc
  prettyProc `shouldBe` src

unwrapRight :: (Show a) => Either a b -> b
unwrapRight e = case e of
  Right x -> x
  Left err -> error (show err)
