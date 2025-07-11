{-# LANGUAGE OverloadedStrings #-}

module CCSCheckerSpec (spec) where

import qualified CCS.Parser
import Data.Function ((&))
import Data.Text (Text)
import qualified Mu.Checker
import Test.Hspec

spec :: Spec
spec = do
  it "no specs" $ do
    let p = "Main = 0"
    verifyProgram p `shouldBe` True

  it "true spec" $ do
    let p =
          "@specs true\
          \Main = 0"
    verifyProgram p `shouldBe` True

  it "false spec" $ do
    let p =
          "@specs false\
          \Main = 0"
    verifyProgram p `shouldBe` False

  it "perform a diamond step on every direction when there are some" $ do
    let p =
          "@specs <true> true\
          \Main = a?.0"
    verifyProgram p `shouldBe` True

  it "perform a diamond step on every direction when there aren't any" $ do
    let p =
          "@specs <a?> true\
          \Main = 0"
    verifyProgram p `shouldBe` False

  it "can perfom an action even through tau" $ do
    let p =
          "@specs <a?> true\
          \Main = x!.a?.0 | x?.0"
    verifyProgram p `shouldBe` True

  it "<false> when no succ" $ do
    let p =
          "@specs <false> true\
          \Main = 0"
    verifyProgram p `shouldBe` True

  it "<false> when no succ sat" $ do
    let p =
          "@specs <false> false\
          \Main = a?.0"
    verifyProgram p `shouldBe` True

  it "<false> when some succ sat" $ do
    let p =
          "@specs <false> true\
          \Main = 0"
    verifyProgram p `shouldBe` True

  it "<true> when there aren't any steps" $ do
    let p =
          "@specs <true> true\
          \Main = 0"
    verifyProgram p `shouldBe` False

  it "<true> accepts any path" $ do
    let p =
          "@specs <true> true\
          \Main = a?.0"
    verifyProgram p `shouldBe` True

  it "<true> when it leads to false" $ do
    let p =
          "@specs <true> false\
          \Main = a?.0"
    verifyProgram p `shouldBe` False

  it "<a?> when there is no such step" $ do
    let p =
          "@specs <a?> true\
          \Main = x!.0"
    verifyProgram p `shouldBe` False

  it "handles parametric args, when args match" $ do
    let p =
          "@specs <f(a, b)!> true\
          \Main = f(a, b)!.0"
    verifyProgram p `shouldBe` True

  it "handles parametric args, when args do not match" $ do
    let p =
          "@specs <f(a, b)!> true\
          \Main = f(a, z)!.0"
    verifyProgram p `shouldBe` False

  it "handles substitution of parametric args" $ do
    let p =
          "P(x) = f(x)!.0\
          \@specs <f(a)!> true\
          \Main = P(a)"
    verifyProgram p `shouldBe` True

  it "nested diamonds when leads to true" $ do
    let p =
          "@specs <a!> <b!> true\
          \Main = a!.b!.0"
    verifyProgram p `shouldBe` True

  it "nested diamonds when leads to false" $ do
    let p =
          "@specs <a!> <b!> false\
          \Main = a!.b!.0"
    verifyProgram p `shouldBe` False

  it "box (desugared) when there are no succs" $ do
    let p =
          "@specs ! <a!> true\
          \Main = 0"
    verifyProgram p `shouldBe` True

  it "box when there are no succs" $ do
    let p =
          "@specs [a!] false\
          \Main = 0"
    verifyProgram p `shouldBe` True

  describe "fix point operator" $ do
    it "behaves as the body when binding is unused" $ do
      let p =
            "@specs mu x . true\
            \Main = 0"
      verifyProgram p `shouldBe` True

    it "the binding is false initially" $ do
      let p =
            "@specs mu x . x \
            \Main = 0"
      verifyProgram p `shouldBe` False

    it "the binding is false initially" $ do
      let p =
            "@specs mu x . x \
            \Main = 0"
      verifyProgram p `shouldBe` False

    it "allows to navigate forward" $ do
      -- TODO fix parser
      let p =
            "@specs mu x . (<a!> true || <a?> x) \
            \Main = a?.a?.a!.0"
      verifyProgram p `shouldBe` True

    it "detects loops" $ do
      let p =
            "P = a?.a?.P \
            \@specs mu x . <a!> true || <a?> x \
            \Main = P"
      verifyProgram p `shouldBe` False

    it "travarses recursive (but finite) states" $ do
      let p =
            "BoolRegT = read_t!.BoolRegT + write_t?.BoolRegT + write_f?.BoolRegF \
            \BoolRegF = read_f!.BoolRegF + write_t?.BoolRegT + write_f?.BoolRegF \
            \@specs nu p . <true> true && [true] p \
            \Main = BoolRegT"
      verifyProgram p `shouldBe` True

verifyProgram :: Text -> Bool
verifyProgram src =
  src
    & CCS.Parser.parse "test"
    & unwrapRight
    & Mu.Checker.verifyProgram
    & all (\(_, failedSpecs) -> null $ unwrapRight failedSpecs)

unwrapRight :: (Show a) => Either a b -> b
unwrapRight e = case e of
  Right x -> x
  Left err -> error (show err)
