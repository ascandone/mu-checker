module Mu.Formula (
  Formula (..),
  FormulaEvent (..),
  Evt (..),
  lor,
  always,
  imply,
  box,
  evtOr,
  evtBottom,
  nu,
  mapBinding,
) where

import qualified Data.String
import Data.Text (Text)
import qualified Data.Text as Test

data Evt
  = Rcv Text [Text]
  | Snd Text [Text]
  | Tau
  deriving (Show, Eq, Ord)

data FormulaEvent
  = Evt Evt
  | Up
  | EvtAnd FormulaEvent FormulaEvent
  | EvtNot FormulaEvent
  deriving (Show, Eq, Ord)

evtOr :: FormulaEvent -> FormulaEvent -> FormulaEvent
evtOr x y = EvtNot (EvtNot x `EvtAnd` EvtNot y)

evtBottom :: FormulaEvent
evtBottom = EvtNot Up

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
  Not (Mu binding $ Not (negateBinding binding formula))

negateBinding :: Text -> Formula -> Formula
negateBinding = mapBinding Not

mapBinding :: (Formula -> Formula) -> Text -> Formula -> Formula
mapBinding mapper binding formula =
  case formula of
    Atom binding' | binding == binding' -> mapper formula
    Mu binding' _ | binding == binding' -> formula -- here the old binding is shadowed
    Atom _ -> formula
    Bottom -> formula
    And l r -> And (mapBinding_ binding l) (mapBinding_ binding r)
    Not f -> Not (mapBinding_ binding f)
    Diamond evt f -> Diamond evt (mapBinding_ binding f)
    Mu binding' f' -> Mu binding' (mapBinding_ binding f')
 where
  mapBinding_ = mapBinding mapper

instance Data.String.IsString Formula where
  fromString = Atom . Test.pack
