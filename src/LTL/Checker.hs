{-# LANGUAGE OverloadedRecordDot #-}

module LTL.Checker (check, VerificationResult (..)) where

import qualified Control.Monad
import qualified Control.Monad.State.Strict as State
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe
import Data.Text (Text)
import Kripke (Kripke)
import qualified Kripke
import qualified LTL.Formula as LTL

-- | Check that the formula is verified in the given Kripke structure
check :: (Eq state, Ord state) => Kripke state Text -> LTL.Formula -> VerificationResult state
check k f = State.evalState (vall <$> vs) initialCache
 where
  vs =
    Control.Monad.forM (Kripke.initial k) $ \s ->
      kripke s `verifyAt` f

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

data VisitingState state
  = Visited (VerificationResult state)
  | Visiting state

type Cache state = Map (state, LTL.Formula) (VisitingState state)

initialCache :: Cache state
initialCache = Map.empty

type CacheState state = State.State (Cache state)

-- | Cached version
verifyAt :: (Eq state, Ord state) => KripkeState state Text -> LTL.Formula -> CacheState state (VerificationResult state)
verifyAt k f = do
  cacheLookup <- Map.lookup (k.state, f) <$> State.get
  case cacheLookup of
    Just (Visited v) -> return v
    Just (Visiting _) -> return $ Falsify []
    Nothing -> do
      State.modify $ Map.insert (k.state, f) (Visiting k.state)
      v <- verifyAtRaw k f
      State.modify $ Map.insert (k.state, f) (Visited v)
      return v

-- | Not cached (but recurs on cached version)
verifyAtRaw :: (Eq state, Ord state) => KripkeState state Text -> LTL.Formula -> CacheState state (VerificationResult state)
verifyAtRaw k (LTL.Not (LTL.Not f)) = verifyAtRaw k f
verifyAtRaw k formula =
  appendK <$> case formula of
    LTL.Bottom -> return $ Falsify []
    LTL.And fl fr -> do
      vl <- kVerify fl
      vr <- kVerify fr
      return (vand vl vr)
    LTL.Not f' -> do
      vf' <- kVerify f'
      return $ vnot vf'
    LTL.Atom prop ->
      return $ vbool (k.interpret prop)
    LTL.Next f' -> do
      vs <- Control.Monad.forM k.transitions (`verifyAt` f')
      return $ vall vs
    LTL.Until precondition goal -> do
      -- TODO if monad is strict, does it mean this is always evaluated? double check
      vgoal <- kVerify goal
      vprecondition <- kVerify precondition
      vs <- Control.Monad.forM k.transitions (`verifyAt` formula)
      return $ vgoal `vor` (vprecondition `vand` vany vs)
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
vnot Verify = Falsify []
vnot (Falsify _) = Verify

vall :: [VerificationResult state] -> VerificationResult state
vall xs =
  case Data.Maybe.mapMaybe extractCounterExample xs of
    [] -> Verify
    counterexample : _ -> Falsify counterexample
 where
  extractCounterExample Verify = Nothing
  extractCounterExample (Falsify counterexample) = Just counterexample

vany :: [VerificationResult state] -> VerificationResult state
vany xs = vbool $ any isVerified xs
 where
  isVerified Verify = True
  isVerified (Falsify _) = False
