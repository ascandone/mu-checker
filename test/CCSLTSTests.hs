{-# LANGUAGE OverloadedStrings #-}

module CCSLTSTests (suite) where

import qualified CCS.LTS as LTS
import qualified CCS.Program as CCS
import qualified CCS.Program.Parser as P
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Test.Tasty as Tasty
import qualified Test.Tasty as Tasy
import Test.Tasty.HUnit (testCase, (@?=))

tests :: [Tasy.TestTree]
tests =
  [ testCase "empty program has no transitions" $
      getTransitions [] "0"
        @?= []
  , testCase "unary choice" $
      getTransitions [] "a?.0"
        @?= [ (Just $ CCS.Rcv "a", CCS.Choice [])
            ]
  , testCase "binary choice" $
      getTransitions [] "a?.A + b!.B"
        @?= [ (Just $ CCS.Rcv "a", CCS.Ident "A" [])
            , (Just $ CCS.Snd "b", CCS.Ident "B" [])
            ]
  , testCase "wrapped in par" $
      getTransitions [] "0 | a?.A + b!.B | 0"
        @?= [ (Just $ CCS.Rcv "a", parse "0 | A | 0")
            , (Just $ CCS.Snd "b", parse "0 | B | 0")
            ]
  , testCase "wrapped in par both transitions" $
      getTransitions [] "a?.A | b!.B"
        @?= [ (Just $ CCS.Rcv "a", parse "A | b!.B")
            , (Just $ CCS.Snd "b", parse "a?.A | B")
            ]
  , testCase "handshake" $
      getTransitions [] "a?.X | a!.Y"
        @?= [ (Just $ CCS.Rcv "a", parse "X | a!.Y")
            , (Just $ CCS.Snd "a", parse "a?.X | Y")
            , (Nothing, parse "X | Y")
            ]
  ]

suite :: Tasty.TestTree
suite = Tasty.testGroup "CCSLTsTests" tests

parse :: Text -> CCS.Process
parse = unwrapRight . P.parseProc

getTransitions :: [(Text, CCS.Definition)] -> Text -> [(Maybe CCS.EventChoice, CCS.Process)]
getTransitions lst src =
  unwrapRight $ LTS.getTransitions (Map.fromList lst) (parse src)

unwrapRight :: (Show a) => Either a b -> b
unwrapRight e = case e of
  Right x -> x
  Left err -> error (show err)
