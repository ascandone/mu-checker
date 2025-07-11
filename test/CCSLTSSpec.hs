{-# LANGUAGE OverloadedStrings #-}

module CCSLTSSpec (spec) where

import qualified CCS.LTS as LTS
import qualified CCS.Parser as P
import qualified CCS.Program as CCS
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Test.Hspec

spec :: Spec
spec = do
  it "empty program has no transitions" $ do
    getTransitions [] "0" `shouldBe` []

  it "unary choice" $ do
    getTransitions [] "a?.0"
      `shouldBe` [ (Just $ CCS.Action CCS.Rcv "a" [], CCS.Choice [])
                 ]

  it "binary choice" $ do
    getTransitions [] "a?.A + b!.B"
      `shouldBe` [ (Just $ CCS.Action CCS.Rcv "a" [], CCS.Ident "A" [])
                 , (Just $ CCS.Action CCS.Snd "b" [], CCS.Ident "B" [])
                 ]

  it "wrapped in par" $ do
    getTransitions [] "0 | a?.A + b!.B | 0"
      `shouldBe` [ (Just $ CCS.Action CCS.Rcv "a" [], parse "0 | A | 0")
                 , (Just $ CCS.Action CCS.Snd "b" [], parse "0 | B | 0")
                 ]

  it "wrapped in par both transitions" $ do
    getTransitions [] "a?.A | b!.B"
      `shouldBe` [ (Just $ CCS.Action CCS.Rcv "a" [], parse "A | b!.B")
                 , (Just $ CCS.Action CCS.Snd "b" [], parse "a?.A | B")
                 ]

  it "handshake" $ do
    getTransitions [] "a?.X | a!.Y"
      `shouldBe` [ (Just $ CCS.Action CCS.Rcv "a" [], parse "X | a!.Y")
                 , (Just $ CCS.Action CCS.Snd "a" [], parse "a?.X | Y")
                 , (Nothing, parse "X | Y")
                 ]

  it "parametric transition" $ do
    getTransitions [] "f(a, b)?.X"
      `shouldBe` [ (Just $ CCS.Action CCS.Rcv "f" ["a", "b"], parse "X")
                 ]

parse :: Text -> CCS.Process
parse = unwrapRight . P.parseProc "proc"

getTransitions :: [(Text, CCS.Definition)] -> Text -> [(Maybe CCS.Action, CCS.Process)]
getTransitions lst src =
  unwrapRight $ LTS.getTransitions (Map.fromList lst) (parse src)

unwrapRight :: (Show a) => Either a b -> b
unwrapRight e = case e of
  Right x -> x
  Left err -> error (show err)
