module LTL.Formula (
  Formula (..),
  lor,
  globally,
  always,
  finally,
  imply,
) where

import qualified Data.String
import Data.Text (Text)
import qualified Data.Text as Test

data Formula
  = Bottom
  | Atom Text
  | And Formula Formula
  | Not Formula
  | Next Formula
  | Until Formula Formula
  deriving (Show, Eq, Ord)

always :: Formula
always = Not Bottom

lor :: Formula -> Formula -> Formula
lor x y = Not (Not x `And` Not y)

finally :: Formula -> Formula
finally f = always `Until` f

globally :: Formula -> Formula
globally f = Not (finally (Not f))

imply :: Formula -> Formula -> Formula
imply if_ then_ = Not if_ `lor` then_

instance Data.String.IsString Formula where
  fromString = Atom . Test.pack
