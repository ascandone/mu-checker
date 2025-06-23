module CCS.Program (
  Program,
  Definition (..),
  Process (..),
  -- EventChoice (..),
  ActionType (..),
  Action (..),
) where

import Data.Text (Text)
import qualified Mu.Formula as Mu
import Parser (Ranged)

type Program = [Definition]

data Definition = Definition
  { specs :: [Ranged Mu.Formula]
  , name :: Text
  , params :: [Text]
  , definition :: Process
  }
  deriving (Show, Eq, Ord)

data ActionType
  = Snd
  | Rcv
  deriving (Show, Eq, Ord)

data Action
  = Action ActionType Text
  deriving (Show, Eq, Ord)

data Process
  = Ident Text [Text]
  | Choice [(Action, Process)]
  | Par Process Process
  | Restriction Text Process
  deriving (Show, Eq, Ord)
