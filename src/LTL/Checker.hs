{-# LANGUAGE OverloadedRecordDot #-}

module LTL.Checker (check, VerificationResult (..)) where

import qualified Data.Maybe
import Data.Text (Text)
import Kripke (Kripke)
import qualified Kripke
import qualified LTL.Formula as LTL

-- | Check that the formula is verified in the given Kripke structure
check :: (Eq state, Ord state) => Kripke state Text -> LTL.Formula -> VerificationResult state
check k f =
  vall
    (\s -> kripke s `verifyAt` f)
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

data VerificationResult state
  = Verify
  | Falsify [state]
  deriving (Show, Eq, Ord)

-- TODO fix looping
verifyAt :: (Eq state, Ord state) => KripkeState state Text -> LTL.Formula -> VerificationResult state
verifyAt k formula = appendK $ case formula of
  LTL.Bottom -> vbool False
  LTL.And fl fr -> kVerify fl `vand` kVerify fr
  LTL.Not f' -> vnot (kVerify f')
  LTL.Atom prop -> vbool (k.interpret prop)
  LTL.Next f' -> vall (`verifyAt` f') k.transitions
  LTL.Until condition guard ->
    kVerify guard `vor` (kVerify condition `vand` vall (`verifyAt` formula) k.transitions)
 where
  kVerify = verifyAt k
  appendK Verify = Verify
  appendK (Falsify ks) = Falsify (k.state : ks)

vbool :: Bool -> VerificationResult state
vbool True = Verify
vbool False = Falsify []

vand :: VerificationResult state -> VerificationResult state -> VerificationResult state
vand Verify y = y
vand f@(Falsify _) _ = f

vor :: VerificationResult state -> VerificationResult state -> VerificationResult state
vor Verify _ = Verify
vor (Falsify _) y = y

vnot :: VerificationResult state -> VerificationResult state
vnot Verify = vbool False
vnot (Falsify _) = vbool True

vall :: (a -> VerificationResult state) -> [a] -> VerificationResult state
vall predicate xs =
  case Data.Maybe.mapMaybe extractCounterExample xs of
    [] -> Verify
    counterexample : _ -> Falsify counterexample
 where
  extractCounterExample res = case predicate res of
    Verify -> Nothing
    Falsify counterexample -> Just counterexample
