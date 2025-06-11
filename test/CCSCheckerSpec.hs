{-# LANGUAGE OverloadedStrings #-}

module CCSCheckerSpec (spec) where

import qualified CCS.Checker
import qualified CCS.Parser
import Data.Function ((&))
import Data.Text (Text)
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

verifyProgram :: Text -> Bool
verifyProgram src =
  src
    & CCS.Parser.parse "test"
    & unwrapRight
    & CCS.Checker.verifyProgram
    & all (\(_, failedSpecs) -> null $ unwrapRight failedSpecs)

unwrapRight :: (Show a) => Either a b -> b
unwrapRight e = case e of
  Right x -> x
  Left err -> error (show err)
