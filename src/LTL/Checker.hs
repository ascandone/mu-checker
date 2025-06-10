{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}

module LTL.Checker (check) where

import Data.Text (Text)
import Kripke (Kripke)
import qualified Kripke
import qualified LTL.Formula as LTL

-- | Check that the formula is verified in the given Kripke structure
check :: (Eq state, Ord state) => Kripke state Text -> LTL.Formula -> Bool
check k f =
  all
    (\s -> kripke s `checkAt` f)
    (Kripke.initial k)
 where
  kripke s =
    KripkeState
      { state = s
      , transitions = map kripke (k.transitions s)
      , interpret = \p -> k.interpret p s
      }

-- | A single state of a Kripke structure
data KripkeState state prop = KripkeState
  { state :: state
  , transitions :: [KripkeState state prop]
  , interpret :: prop -> Bool
  }

-- TODO fix looping
checkAt :: (Eq state, Ord state) => KripkeState state Text -> LTL.Formula -> Bool
checkAt k formula =
  case formula of
    LTL.Bottom -> False
    LTL.And fl fr -> k `checkAt` fl && k `checkAt` fr
    LTL.Not f' -> not (k `checkAt` f')
    LTL.Atom prop -> k.interpret prop
    LTL.Next f' -> all (`checkAt` f') k.transitions
    LTL.Until _ guard | k `checkAt` guard -> True
    LTL.Until condition _ | not (k `checkAt` condition) -> False
    LTL.Until _ _ -> all (`checkAt` formula) k.transitions
