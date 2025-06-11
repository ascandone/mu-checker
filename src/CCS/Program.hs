module CCS.Program (
  Program,
  Definition (..),
  Process (..),
  EventChoice (..),
) where

import Data.Text (Text)
import qualified Mu.Formula as Mu

type Program = [Definition]

data Definition = Definition
  { specs :: [Mu.Formula]
  , name :: Text
  , params :: [Text]
  , definition :: Process
  }
  deriving (Show, Eq, Ord)

data EventChoice
  = Snd Text
  | Rcv Text
  deriving (Show, Eq, Ord)

data Process
  = Ident Text [Text]
  | Choice [(EventChoice, Process)]
  | Par Process Process
  | Restriction Text Process
  deriving (Show, Eq, Ord)
