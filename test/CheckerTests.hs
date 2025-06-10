{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module CheckerTests (suite) where

import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Kripke
import LTL.Checker (check)
import LTL.Formula (Formula (..), always)
import qualified LTL.Formula as LTL
import qualified Test.Tasty as Tasty
import Test.Tasty.HUnit (testCase, (@?=))

tests :: [CheckerTestCase]
tests =
  [ CheckerTestCase
      { label = "true when no staets"
      , formula = always
      , expected = True
      , initial = []
      , transitions = []
      , interpret = []
      }
  , CheckerTestCase
      { label = "false for bottom"
      , formula = Bottom
      , expected = False
      , initial = ["a"]
      , transitions = []
      , interpret = []
      }
  , CheckerTestCase
      { label = "fail predicate when doesn't satisfy state"
      , formula = "x"
      , expected = False
      , initial = ["a"]
      , transitions = []
      , interpret = []
      }
  , CheckerTestCase
      { label = "succeeds predicate when doesn't satisfy state"
      , formula = "x"
      , expected = True
      , initial = ["a"]
      , transitions = []
      , interpret = [("x", ["a"])]
      }
  , CheckerTestCase
      { label = "next should be sat on next state"
      , formula = Next "x"
      , expected = True
      , initial = ["a"]
      , transitions =
          [ ("a", ["b"])
          ]
      , interpret = [("x", ["b"])]
      }
  , CheckerTestCase
      { label = "next should be sat on _all_ next states"
      , formula = Next "x"
      , expected = False
      , initial = ["a"]
      , transitions =
          [ ("a", ["b", "does_not_sat_x"])
          ]
      , interpret = [("x", ["b"])]
      }
  , CheckerTestCase
      { label = "x U y fails when x stop being true before y is true"
      , formula = "x" `Until` "y" -- a(x) -> b() -> c(y)
      , expected = False
      , initial = ["a"]
      , transitions =
          [ ("a", ["b"])
          , ("b", ["c"])
          ]
      , interpret =
          [ ("x", ["a"])
          , ("y", ["c"])
          ]
      }
  , CheckerTestCase
      { label = "x U y succeeds if y start being true even if x is not anymore"
      , formula = "x" `Until` "y" -- a(x) -> b(x) -> c(y)
      , expected = True
      , initial = ["a"]
      , transitions =
          [ ("a", ["b"])
          , ("b", ["c"])
          ]
      , interpret =
          [ ("x", ["a", "b"])
          , ("y", ["c"])
          ]
      }
  ]

suite :: Tasty.TestTree
suite = fromTC $ Group "CheckerTests" tests

data CheckerTestCase
  = CheckerTestCase
      { label :: String
      , formula :: LTL.Formula
      , expected :: Bool
      , initial :: [Text]
      , transitions :: [(Text, [Text])]
      , interpret :: [(Text, [Text])]
      }
  | Group String [CheckerTestCase]

fromTC :: CheckerTestCase -> Tasty.TestTree
fromTC (Group label_ tests_) = Tasty.testGroup label_ (map fromTC tests_)
fromTC tc = testCase tc.label $ check k tc.formula @?= tc.expected
 where
  transitionsGraph = Map.fromList tc.transitions
  interpretationGraph = Map.fromList tc.interpret
  transitions' s = Maybe.fromMaybe [] $ Map.lookup s transitionsGraph
  interpret' prop state = case Map.lookup prop interpretationGraph of
    Nothing -> False
    Just xs -> state `elem` xs
  k =
    Kripke.Kripke
      { Kripke.initial = tc.initial
      , Kripke.transitions = transitions'
      , Kripke.interpret = interpret'
      }
