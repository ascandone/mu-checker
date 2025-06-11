module Mu.Formula (
  Formula (..),
  lor,
  always,
  imply,
  box,
  evtOr,
  evtAlways,
) where

import qualified Data.String
import Data.Text (Text)
import qualified Data.Text as Test

data FormulaEvent
  = Rcv Text
  | Snd Text
  | Tau
  | Bottom_
  | And_ FormulaEvent FormulaEvent
  | Not_ FormulaEvent
  deriving (Show, Eq, Ord)

evtOr :: FormulaEvent -> FormulaEvent -> FormulaEvent
evtOr x y = Not_ (Not_ x `And_` Not_ y)

evtAlways :: FormulaEvent
evtAlways = Not_ Bottom_

data Formula
  = Bottom
  | Atom Text
  | And Formula Formula
  | Not Formula
  | Diamond FormulaEvent Formula
  | Mu Text Formula
  | Nu Text Formula
  deriving (Show, Eq, Ord)

always :: Formula
always = Not Bottom

box :: FormulaEvent -> Formula -> Formula
box evt f = Not (Diamond evt (Not f))

lor :: Formula -> Formula -> Formula
lor x y = Not (Not x `And` Not y)

imply :: Formula -> Formula -> Formula
imply if_ then_ = Not if_ `lor` then_

instance Data.String.IsString Formula where
  fromString = Atom . Test.pack