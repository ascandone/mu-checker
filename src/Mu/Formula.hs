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