{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module LTLCheckerSpec (spec) where

import qualified Control.Monad
import qualified Data.Either
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import LTL.Checker (VerificationResult (..), check)
import qualified LTL.Checker
import LTL.Formula (Formula)
import qualified LTL.Formula as LTL
import qualified LTL.Parser as P
import Test.Hspec

spec :: Spec
spec =
  Control.Monad.forM_ tests $ \testCase ->
    fromTC testCase

tests :: [CheckerTestCase]
tests =
  [ CheckerTestCase
      { label = "true when no staets"
      , formula = parse "true"
      , expected = Verify
      , initial = []
      , transitions = []
      , interpret = []
      }
  , CheckerTestCase
      { label = "false for bottom"
      , formula = parse "false"
      , expected = Falsify ["a"]
      , initial = ["a"]
      , transitions = []
      , interpret = []
      }
  , CheckerTestCase
      { label = "fail predicate when doesn't satisfy state"
      , formula = parse "x"
      , expected = Falsify ["a"]
      , initial = ["a"]
      , transitions = []
      , interpret = []
      }
  , CheckerTestCase
      { label = "succeeds predicate when doesn't satisfy state"
      , formula = parse "x"
      , expected = Verify
      , initial = ["a"]
      , transitions = []
      , interpret = [("x", ["a"])]
      }
  , CheckerTestCase
      { label = "next should be sat on next state"
      , formula = parse "X x"
      , expected = Verify
      , initial = ["a"]
      , transitions =
          [ ("a", ["b"])
          ]
      , interpret = [("x", ["b"])]
      }
  , CheckerTestCase
      { label = "next should be sat on _all_ next states"
      , formula = parse "X x"
      , expected = Falsify ["a", "does_not_sat_x"]
      , initial = ["a"]
      , transitions =
          [ ("a", ["b", "does_not_sat_x"])
          ]
      , interpret = [("x", ["b"])]
      }
  , CheckerTestCase
      { label = "unwrap the double Not"
      , formula = parse "! ! (X x)"
      , expected = Falsify ["a", "does_not_sat_x"]
      , initial = ["a"]
      , transitions =
          [ ("a", ["b", "does_not_sat_x"])
          ]
      , interpret = [("x", ["b"])]
      }
  , CheckerTestCase
      { label = "x U y fails when x stops being true before y is true"
      , formula = parse "x U y" -- a(x) -> b() -> c(y)
      , expected = Falsify ["a"]
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
      , formula = parse "x U y" -- a(x) -> b(x) -> c(y)
      , expected = Verify
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
  , CheckerTestCase
      { label = "Until succeeds if there are no successors"
      , formula = parse "x U y"
      , expected = Falsify ["a"]
      , initial = ["a"]
      , transitions =
          []
      , interpret =
          [ ("x", ["a"])
          ]
      }
  , CheckerTestCase
      { label = "Until fails if y never holds"
      , formula = parse "x U y" -- a(x) -> b(x) -> c(x)
      , expected = Falsify ["a"]
      , initial = ["a"]
      , transitions =
          [ ("a", ["b"])
          , ("b", ["c"])
          ]
      , interpret =
          [ ("x", ["a", "b", "c"])
          ]
      }
  , CheckerTestCase
      { label = "finally succeds if the property is reached at a certain point"
      , formula = parse "F x" -- a -> b -> c(x)
      , expected = Verify
      , initial = ["a"]
      , transitions =
          [ ("a", ["b"])
          , ("b", ["c"])
          ]
      , interpret =
          [ ("x", ["c"])
          ]
      }
  , CheckerTestCase
      { label = "finally fails if the property is never reached"
      , formula = parse "F x" -- a -> b -> c
      , expected = Falsify ["a"]
      , initial = ["a"]
      , transitions =
          [ ("a", ["b"])
          , ("b", ["c"])
          ]
      , interpret =
          []
      }
  , CheckerTestCase
      { label = "G x works if all states have x"
      , formula = parse "G x" -- a(x) -> b(x) -> c(x)
      , expected = Verify
      , initial = ["a"]
      , transitions =
          [ ("a", ["b"])
          , ("b", ["c"])
          ]
      , interpret =
          [ ("x", ["a", "b", "c"])
          ]
      }
  , CheckerTestCase
      { label = "F x does not work if no state have x, and there's a loop"
      , formula = parse "F x" -- a -> b -> c
      , expected = Falsify ["a"]
      , initial = ["a"]
      , transitions =
          [ ("a", ["b"])
          , ("b", ["c"])
          , ("c", ["a"])
          ]
      , interpret = []
      }
  , CheckerTestCase
      { label = "G x works if all states have x, and there's a loop"
      , formula = parse "G x" -- a(x) -> b(x) -> c(x)
      , expected = Verify
      , initial = ["a"]
      , transitions =
          [ ("a", ["b"])
          , ("b", ["c"])
          , ("c", ["a"])
          ]
      , interpret =
          [ ("x", ["a", "b", "c"])
          ]
      }
  ]

data CheckerTestCase
  = CheckerTestCase
  { label :: String
  , formula :: LTL.Formula
  , expected :: VerificationResult Text
  , initial :: [Text]
  , transitions :: [(Text, [Text])]
  , interpret :: [(Text, [Text])]
  }

fromTC :: CheckerTestCase -> SpecWith ()
fromTC tc = it tc.label $ check k tc.formula `shouldBe` tc.expected
 where
  transitionsGraph = Map.fromList tc.transitions
  interpretationGraph = Map.fromList tc.interpret
  transitions' s = Maybe.fromMaybe [] $ Map.lookup s transitionsGraph
  interpret' prop state = case Map.lookup prop interpretationGraph of
    Nothing -> False
    Just xs -> state `elem` xs
  k =
    LTL.Checker.Kripke
      { LTL.Checker.initial = tc.initial
      , LTL.Checker.transitions = transitions'
      , LTL.Checker.interpret = interpret'
      }

parse :: Text -> Formula
parse = Data.Either.fromRight (error "fromRight") . P.parse
