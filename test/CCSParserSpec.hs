{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant $" #-}
module CCSParserSpec (spec) where

import CCS.Parser (errorBundlePretty)
import qualified CCS.Parser as P
import CCS.Program (ActionType (..), Definition (..), Process (..))
import qualified CCS.Program as CCS
import qualified Data.Text as Text
import qualified Mu.Formula as Mu
import qualified Parser
import Test.Hspec

spec :: Spec
spec = do
  testParseProcess "0" $
    Choice []

  testParseProcess "0\\a" $
    Restriction "a" (Choice [])

  testParseProcess "0\\{a, b}" $
    Restriction "a" $
      Restriction "b" (Choice [])

  testParseProcess "X\\{a, b}" $
    Restriction "a" $
      Restriction "b" (Ident "X" [])

  testParseProcess "(X)\n\\{\na, b}" $
    Restriction "a" $
      Restriction "b" (Ident "X" [])

  testParseProcess "f(a, b)?.0" $
    Choice
      [ (CCS.Action Rcv "f" ["a", "b"], Choice [])
      ]

  testParseProcess "(a? + b!).P" $
    Choice
      [ (CCS.Action Rcv "a" [], Ident "P" [])
      , (CCS.Action Snd "b" [], Ident "P" [])
      ]

  testParseProgram "Main = 0" $
    [ Definition
        { name = "Main"
        , params = []
        , definition = Choice []
        , specs = []
        }
    ]
  testParseProgram "Main = X" $
    [ Definition
        { name = "Main"
        , params = []
        , definition = Ident "X" []
        , specs = []
        }
    ]
  testParseProgram "Main = X(a, b)" $
    [ Definition
        { name = "Main"
        , params = []
        , definition = Ident "X" ["a", "b"]
        , specs = []
        }
    ]
  testParseProgram "Main(arg) = 0" $
    [ Definition
        { name = "Main"
        , params = ["arg"]
        , definition = Choice []
        , specs = []
        }
    ]
  testParseProgram "P = X | Y" $
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
  testParseProgram "P = X | (Y | Z)" $
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
  testParseProgram "P = a?.0" $
    [ Definition
        { name = "P"
        , params = []
        , specs = []
        , definition =
            Choice
              [ (CCS.Action Rcv "a" [], Choice [])
              ]
        }
    ]
  testParseProgram "P = a?.0 | b!.0" $ --  TODO parens
    [ Definition
        { name = "P"
        , params = []
        , specs = []
        , definition =
            Par
              ( Choice
                  [ (CCS.Action Rcv "a" [], Choice [])
                  ]
              )
              ( Choice
                  [ (CCS.Action Snd "b" [], Choice [])
                  ]
              )
        }
    ]
  testParseProgram "P = a?.0 + b!.0" $
    [ Definition
        { name = "P"
        , params = []
        , specs = []
        , definition =
            Choice
              [ (CCS.Action Rcv "a" [], Choice [])
              , (CCS.Action Snd "b" [], Choice [])
              ]
        }
    ]
  testParseProgram "P = a?.0 + b!.0 | X" $
    [ Definition
        { name = "P"
        , params = []
        , specs = []
        , definition =
            Par
              ( Choice
                  [ (CCS.Action Rcv "a" [], Choice [])
                  , (CCS.Action Snd "b" [], Choice [])
                  ]
              )
              ( Ident "X" []
              )
        }
    ]
  testParseProgram "P = a?.0 + b!.(X | Y)" $
    [ Definition
        { name = "P"
        , params = []
        , specs = []
        , definition =
            Choice
              [ (CCS.Action Rcv "a" [], Choice [])
              ,
                ( CCS.Action Snd "b" []
                , Par
                    (Ident "X" [])
                    (Ident "Y" [])
                )
              ]
        }
    ]
  testParseProgram "P = (a?.0 + b!.X) | Y" $
    [ Definition
        { name = "P"
        , params = []
        , specs = []
        , definition =
            Par
              ( Choice
                  [ (CCS.Action Rcv "a" [], Choice [])
                  ,
                    ( CCS.Action Snd "b" []
                    , Ident "X" []
                    )
                  ]
              )
              (Ident "Y" [])
        }
    ]
  testParseProgram "P = A\nQ = B" $
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
  testParseProgram "@specs <false> x\nP = 0 " $
    [ Definition
        { name = "P"
        , params = []
        , specs =
            [ Parser.Ranged
                (Parser.Range (Parser.Position 1 1) (Parser.Position 2 1))
                ( Mu.Diamond Mu.evtBottom "x"
                )
            ]
        , definition = Choice []
        }
    ]

testParseProgram :: String -> CCS.Program -> SpecWith ()
testParseProgram src expected =
  it (unescape src) $ case P.parse "test" (Text.pack src) of
    Left e -> error $ errorBundlePretty e
    Right p -> p `shouldBe` expected

testParseProcess :: String -> CCS.Process -> SpecWith ()
testParseProcess src expected =
  it (unescape src) $ case P.parseProc "test" (Text.pack src) of
    Left e -> error $ errorBundlePretty e
    Right p -> p `shouldBe` expected

unescape :: String -> String
unescape =
  concatMap
    ( \ch ->
        if ch == '\n'
          then "\\n"
          else [ch]
    )
