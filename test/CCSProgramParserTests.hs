{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant $" #-}
module CCSProgramParserTests (suite) where

import CCS.Program (Definition (..), Process (..), Program)
import qualified CCS.Program.Parser as P
import qualified Data.Text as Text
import Test.Tasty (testGroup)
import qualified Test.Tasty as Tasty
import Test.Tasty.HUnit ((@?=))
import qualified Test.Tasty.HUnit

testCase :: String -> Program -> Tasty.TestTree
testCase src expected =
  Test.Tasty.HUnit.testCase
    src
    (P.parse (Text.pack src) @?= Right expected)

tests :: [Tasty.TestTree]
tests =
  [ testCase "Main = 0" $
      [ Definition
          { name = "Main"
          , params = []
          , definition = Choice []
          , specs = []
          }
      ]
  , testCase "Main = X" $
      [ Definition
          { name = "Main"
          , params = []
          , definition = Ident "X" []
          , specs = []
          }
      ]
  , testCase "Main = X(a, b)" $
      [ Definition
          { name = "Main"
          , params = []
          , definition = Ident "X" ["a", "b"]
          , specs = []
          }
      ]
  , testCase "Main(arg) = 0" $
      [ Definition
          { name = "Main"
          , params = ["arg"]
          , definition = Choice []
          , specs = []
          }
      ]
  , testCase "P = X | Y" $
      [ Definition
          { name = "P"
          , params = []
          , specs = []
          , definition =
              Par
                (Ident "X" [])
                (Ident "Y" [])
          }
      ]
  ]

suite :: Tasty.TestTree
suite = testGroup "CCSProgramParserTests" tests
