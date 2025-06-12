{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Mu.Checker (
  verifyProgram,
  FailingSpec (..),
) where

import qualified CCS.LTS as LTS
import qualified CCS.Program as CCS
import qualified Control.Monad
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

type Cache = Map (CCS.Process, Text, Mu.Formula, Set CCS.Process) VisitingState

initialCache :: Cache
initialCache = Map.empty

type CacheStateT = State.StateT Cache (Either LTS.Err)

type DefinitionsMap = Map Text CCS.Definition
type MuEnv = Map Text (Set CCS.Process)

newtype FailingSpec
  = FalsifiedFormula Mu.Formula

-- | All the new states reachable from this state that satisfy the formula (given this approx)
improveApprox :: LTS -> Text -> Mu.Formula -> Set CCS.Process -> CacheStateT (Set CCS.Process)
improveApprox = cache $ \lts binding formula approx -> do
  let muEnv = Map.insert binding approx Map.empty
  verified <- verify muEnv lts formula

  transitions <- State.lift $ getTransitions lts -- TODO this could be cached in the LTS struct?
  vs <- Control.Monad.forM transitions $ \(_evt, lts') ->
    improveApprox lts' binding formula approx

  let base =
        if verified
          then lts._process `Set.insert` approx
          else approx
  return $ foldr Set.union base vs

verify :: MuEnv -> LTS -> Mu.Formula -> CacheStateT Bool
verify muEnv lts formula = do
  let verify_ = verify muEnv lts
  transitions <- State.lift $ getTransitions lts
  case formula of
    Mu.Not (Mu.Not formula') -> verify muEnv lts formula'
    Mu.Bottom -> return False
    Mu.And l r -> do
      l' <- verify_ l
      r' <- verify_ r
      return (l' && r')
    Mu.Atom bin -> return $ case Map.lookup bin muEnv of
      Nothing -> False
      Just s -> lts._process `Set.member` s
    Mu.Not formula' -> do
      b <- verify_ formula'
      return $ not b
    Mu.Mu binding body -> do
      -- TODO refactor as "findFixPoint" for perf reasons
      fixPoint <- findGreatestFixpoint (improveApprox lts binding body) Set.empty
      return $ lts._process `Set.member` fixPoint
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

getTransitions :: LTS -> Either LTS.Err [(Mu.Evt, LTS)]
getTransitions lts = do
  transitions <- LTS.getTransitions lts._defsMap lts._process
  return [(mapChoice evt, lts{_process = proc_}) | (evt, proc_) <- transitions]

mapChoice :: Maybe CCS.EventChoice -> Mu.Evt
mapChoice evt =
  case evt of
    Nothing -> Mu.Tau
    Just (CCS.Rcv e) -> Mu.Rcv e
    Just (CCS.Snd e) -> Mu.Snd e

verifyProgram :: CCS.Program -> [(CCS.Definition, Either LTS.Err [FailingSpec])]
verifyProgram definitions =
  [ (def, fst <$> State.runStateT (verifyDefinitionSpecs defsMap def) initialCache)
  | def <- definitions
  ]
 where
  defsMap :: DefinitionsMap
  defsMap = Map.fromList [(def.name, def) | def <- definitions]

data LTS
  = State
  { _process :: CCS.Process
  , _defsMap :: DefinitionsMap
  }

verifyDefinitionSpecs :: DefinitionsMap -> CCS.Definition -> CacheStateT [FailingSpec]
verifyDefinitionSpecs defsMap_ def = do
  vs <- Control.Monad.forM def.specs $ \(CCS.Ranged () formula) -> do
    let lts = State{_process = def.definition, _defsMap = defsMap_}
    b <- Mu.Checker.verify Map.empty lts formula
    return ([FalsifiedFormula formula | not b])
  return $ concat vs

findGreatestFixpoint :: (Eq t) => (t -> CacheStateT t) -> t -> CacheStateT t
findGreatestFixpoint f x = do
  next <- f x
  if next == x
    then return next
    else findGreatestFixpoint f next

cache ::
  (LTS -> Text -> Mu.Formula -> Set CCS.Process -> CacheStateT (Set CCS.Process)) ->
  LTS ->
  Text ->
  Mu.Formula ->
  Set CCS.Process ->
  CacheStateT (Set CCS.Process)
cache f lts binding formula approx = do
  let key = (lts._process, binding, formula, approx)
  cacheLookup <- State.gets (Map.lookup key)
  case cacheLookup of
    Just (Visited v) -> return v
    Just Visiting -> return Set.empty
    Nothing -> do
      State.modify $ Map.insert key Visiting
      v <- f lts binding formula approx
      State.modify $ Map.insert key (Visited v)
      return v
