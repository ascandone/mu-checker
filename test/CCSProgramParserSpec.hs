{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant $" #-}
module CCSProgramParserSpec (spec) where

import CCS.Parser (errorBundlePretty)
import qualified CCS.Parser as P
import CCS.Program (Definition (..), EventChoice (..), Process (..))
import qualified CCS.Program as CCS
import qualified Data.Text as Text
import qualified Mu.Formula as Mu
import Test.Hspec

spec :: Spec
spec = describe "CCS Parser tests" $ do
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
              [ (Rcv "a", Choice [])
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
                  [ (Rcv "a", Choice [])
                  ]
              )
              ( Choice
                  [ (Snd "b", Choice [])
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
              [ (Rcv "a", Choice [])
              , (Snd "b", Choice [])
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
                  [ (Rcv "a", Choice [])
                  , (Snd "b", Choice [])
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
  testParseProgram "P = (a?.0 + b!.X) | Y" $
    [ Definition
        { name = "P"
        , params = []
        , specs = []
        , definition =
            Par
              ( Choice
                  [ (Rcv "a", Choice [])
                  ,
                    ( Snd "b"
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
            [ CCS.Ranged
                ()
                ( Mu.Diamond Mu.EvtBottom "x"
                )
            ]
        , definition = Choice []
        }
    ]

testParseProgram :: String -> CCS.Program -> SpecWith ()
testParseProgram src expected =
  it src $ case P.parse "test" (Text.pack src) of
    Left e -> error $ errorBundlePretty e
    Right p -> p `shouldBe` expected

testParseProcess :: String -> CCS.Process -> SpecWith ()
testParseProcess src expected =
  it src $ case P.parseProc "test" (Text.pack src) of
    Left e -> error $ errorBundlePretty e
    Right p -> p `shouldBe` expected