module Mu.Formula (
  Formula (..),
  FormulaEvent (..),
  Evt (..),
  lor,
  always,
  imply,
  box,
  evtOr,
  evtAlways,
  nu,
) where

import qualified Data.String
import Data.Text (Text)
import qualified Data.Text as Test

data Evt
  = Rcv Text
  | Snd Text
  | Tau
  deriving (Show, Eq, Ord)

data FormulaEvent
  = Evt Evt
  | EvtBottom
  | EvtAnd FormulaEvent FormulaEvent
  | EvtNot FormulaEvent
  deriving (Show, Eq, Ord)

evtOr :: FormulaEvent -> FormulaEvent -> FormulaEvent
evtOr x y = EvtNot (EvtNot x `EvtAnd` EvtNot y)

evtAlways :: FormulaEvent
evtAlways = EvtNot EvtBottom

data Formula
  = Bottom
  | Atom Text
  | And Formula Formula
  | Not Formula
  | Diamond FormulaEvent Formula
  | Mu Text Formula
  deriving (Show, Eq, Ord)

always :: Formula
always = Not Bottom

box :: FormulaEvent -> Formula -> Formula
box evt f = Not (Diamond evt (Not f))

lor :: Formula -> Formula -> Formula
lor x y = Not (Not x `And` Not y)

imply :: Formula -> Formula -> Formula
imply if_ then_ = Not if_ `lor` then_

nu :: Text -> Formula -> Formula
nu binding formula =
  Not (Mu binding (negateBinding binding formula))

negateBinding :: Text -> Formula -> Formula
negateBinding binding formula =
  case formula of
    Atom binding' | binding == binding' -> Not formula
    Atom _ -> formula
    Bottom -> formula
    And l r -> And (negateBinding binding l) (negateBinding binding r)
    Not f -> Not (negateBinding binding f)
    Diamond evt f -> Diamond evt (negateBinding binding f)
    Mu binding' _ | binding == binding' -> formula -- here the old binding is shadowed
    Mu binding' f' -> Mu binding' (negateBinding binding f')

instance Data.String.IsString Formula where
  fromString = Atom . Test.pack
