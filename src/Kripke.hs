module Kripke (Kripke (..)) where

data Kripke s prop = Kripke
  { initial :: [s]
  , transitions :: s -> [s]
  , interpret :: prop -> s -> Bool
  }
