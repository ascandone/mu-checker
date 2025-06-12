{-# LANGUAGE OverloadedRecordDot #-}

module Mu.Checker (
  verifyProgram,
  FailingSpec (..),
) where

import qualified CCS.LTS as LTS
import qualified CCS.Program as CCS
import qualified Control.Monad
import Control.Monad.Error.Class (liftEither)
import qualified Control.Monad.State as State
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Mu.Formula as Mu

data VisitingState
  = Visited (Set CCS.Process)
  | Visiting

type MuEnv = Map Text (Set CCS.Process)

data State = State
  { approxCache :: Map (MuEnv, CCS.Process, Mu.Formula, Set CCS.Process) VisitingState
  , defsMap :: DefinitionsMap
  }

type StateM = State.StateT State (Either LTS.Err)

type DefinitionsMap = Map Text CCS.Definition

newtype FailingSpec
  = FalsifiedFormula Mu.Formula

cacheApprox ::
  (MuEnv -> CCS.Process -> Mu.Formula -> Set CCS.Process -> StateM (Set CCS.Process)) ->
  MuEnv ->
  CCS.Process ->
  Mu.Formula ->
  Set CCS.Process ->
  StateM (Set CCS.Process)
cacheApprox f muEnv proc_ formula approx = do
  let key = (muEnv, proc_, formula, approx)
  cacheLookup <- State.gets (\s -> Map.lookup key s.approxCache)
  case cacheLookup of
    Just (Visited v) -> return v
    Just Visiting -> return Set.empty
    Nothing -> do
      State.modify $ \s -> s{approxCache = Map.insert key Visiting s.approxCache}
      v <- f muEnv proc_ formula approx
      State.modify $ \s -> s{approxCache = Map.insert key (Visited v) s.approxCache}
      return v

-- | All the new states reachable from this state that satisfy the formula (given this approx)
improveApprox :: MuEnv -> CCS.Process -> Mu.Formula -> Set CCS.Process -> StateM (Set CCS.Process)
improveApprox = cacheApprox $ \muEnv proc_ formula approx -> do
  let go newProc = improveApprox muEnv newProc formula approx
  verified <- verify muEnv proc_ formula
  transitions <- getTransitions proc_
  vs <- Control.Monad.forM transitions $ \(_evt, proc') ->
    go proc'
  let base =
        if verified
          then proc_ `Set.insert` approx
          else approx
  return $ foldr Set.union base vs

verify :: MuEnv -> CCS.Process -> Mu.Formula -> StateM Bool
verify muEnv proc_ formula = do
  let verify_ = verify muEnv proc_
  transitions <- getTransitions proc_
  case formula of
    Mu.Not (Mu.Not formula') -> verify muEnv proc_ formula'
    Mu.Bottom -> return False
    Mu.And l r -> do
      l' <- verify_ l
      r' <- verify_ r
      return (l' && r')
    Mu.Atom bin -> return $ case Map.lookup bin muEnv of
      Nothing -> False
      Just s -> proc_ `Set.member` s
    Mu.Not formula' -> do
      b <- verify_ formula'
      return $ not b
    Mu.Mu binding body -> do
      -- TODO refactor as "findFixPoint" for perf reasons
      fixPoint <- findGreatestFixpoint Set.empty $ \currentApprox ->
        let muEnv' = Map.insert binding currentApprox muEnv
         in improveApprox muEnv' proc_ body currentApprox
      return $ proc_ `Set.member` fixPoint
    Mu.Diamond evt formula' ->
      case evt of
        Mu.EvtAnd l r -> do
          l' <- verify_ (Mu.Diamond l formula')
          r' <- verify_ (Mu.Diamond r formula')
          return (l' && r')
        Mu.EvtNot evt' -> do
          b <- verify_ (Mu.Diamond evt' formula')
          return $ not b
        Mu.Up -> do
          bools <- Control.Monad.forM transitions $ \(_, lts') ->
            verify muEnv lts' formula'
          return $ or bools
        Mu.Evt evt' -> do
          bools <- Control.Monad.forM transitions $ \(evt'', lts') -> case evt'' of
            -- we verify again the <> formula (not formula')
            Mu.Tau | evt' /= Mu.Tau -> verify muEnv lts' formula
            _ -> do
              b <- verify muEnv lts' formula'
              return $ evt' == evt'' && b
          return $ or bools

getTransitions :: CCS.Process -> StateM [(Mu.Evt, CCS.Process)]
getTransitions proc_ = do
  defsMap_ <- State.gets defsMap
  transitions <- liftEither $ LTS.getTransitions defsMap_ proc_
  return [(mapChoice evt, proc_') | (evt, proc_') <- transitions]

-- TODO use the same datatype to avoid casting
mapChoice :: Maybe CCS.EventChoice -> Mu.Evt
mapChoice evt =
  case evt of
    Nothing -> Mu.Tau
    Just (CCS.Rcv e) -> Mu.Rcv e
    Just (CCS.Snd e) -> Mu.Snd e

verifyProgram :: CCS.Program -> [(CCS.Definition, Either LTS.Err [FailingSpec])]
verifyProgram definitions =
  [ (def, fst <$> State.runStateT (verifyDefinitionSpecs def) initialState)
  | def <- definitions
  ]
 where
  initialState =
    State
      { defsMap = Map.fromList [(def.name, def) | def <- definitions]
      , approxCache = Map.empty
      }

verifyDefinitionSpecs :: CCS.Definition -> StateM [FailingSpec]
verifyDefinitionSpecs def = do
  vs <- Control.Monad.forM def.specs $ \(CCS.Ranged () formula) -> do
    b <- Mu.Checker.verify Map.empty def.definition formula
    return ([FalsifiedFormula formula | not b])
  return $ concat vs

findGreatestFixpoint :: (Eq t, Show t) => t -> (t -> StateM t) -> StateM t
findGreatestFixpoint x f = do
  next <- f x
  if next == x
    then return next
    else findGreatestFixpoint next f
