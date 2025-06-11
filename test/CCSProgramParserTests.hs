{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant $" #-}
module CCSProgramParserTests (suite) where

import CCS.Program (Definition (..), EventChoice (..), Process (..), Program)
import qualified CCS.Program as CCS
import qualified CCS.Program.Parser as P
import qualified Data.Text as Text
import qualified Mu.Formula as Mu
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
  , testCase "P = X | (Y | Z)" $
      [ Definition
          { name = "P"
          , params = []
          , specs = []
          , definition =
              Par
                (Ident "X" [])
                ( Par
                    (Ident "Y" [])
                    (Ident "Z" [])
                )
          }
      ]
  , testCase "P = a?.0" $
      [ Definition
          { name = "P"
          , params = []
          , specs = []
          , definition =
              Choice
                [ (Rcv "a", Choice [])
                ]
          }
      ]
  , testCase "P = a?.0 | b!.0" $ --  TODO parens
      [ Definition
          { name = "P"
          , params = []
          , specs = []
          , definition =
              Par
                ( Choice
                    [ (Rcv "a", Choice [])
                    ]
                )
                ( Choice
                    [ (Snd "b", Choice [])
                    ]
                )
          }
      ]
  , testCase "P = a?.0 + b!.0" $
      [ Definition
          { name = "P"
          , params = []
          , specs = []
          , definition =
              Choice
                [ (Rcv "a", Choice [])
                , (Snd "b", Choice [])
                ]
          }
      ]
  , testCase "P = a?.0 + b!.0 | X" $
      [ Definition
          { name = "P"
          , params = []
          , specs = []
          , definition =
              Par
                ( Choice
                    [ (Rcv "a", Choice [])
                    , (Snd "b", Choice [])
                    ]
                )
                ( Ident "X" []
                )
          }
      ]
  , testCase "P = a?.0 + b!.(X | Y)" $
      [ Definition
          { name = "P"
          , params = []
          , specs = []
          , definition =
              Choice
                [ (Rcv "a", Choice [])
                ,
                  ( Snd "b"
                  , Par
                      (Ident "X" [])
                      (Ident "Y" [])
                  )
                ]
          }
      ]
  , testCase "P = A\nQ = B" $
      [ Definition
          { name = "P"
          , params = []
          , specs = []
          , definition = Ident "A" []
          }
      , Definition
          { name = "Q"
          , params = []
          , specs = []
          , definition = Ident "B" []
          }
      ]
  , testCase "@specs <false> x\nP = 0 " $
      [ Definition
          { name = "P"
          , params = []
          , specs =
              [ CCS.Ranged
                  ()
                  ( Mu.Diamond Mu.EvtBottom "x"
                  )
              ]
          , definition = Choice []
          }
      ]
  ]

suite :: Tasty.TestTree
suite = testGroup "CCSProgramParserTests" tests
