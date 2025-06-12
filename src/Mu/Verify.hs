{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Mu.Verify (
  verifyProgram,
  FailingSpec (..),
) where

import qualified CCS.LTS as LTS
import qualified CCS.Program as CCS
import qualified Control.Monad
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Mu.Formula as Mu

type DefinitionsMap = Map Text CCS.Definition
type MuEnv = Map Text (Set CCS.Process)

newtype FailingSpec
  = FalsifiedFormula Mu.Formula

-- | All the new states reachable from this state that satisfy the formula (given this approx)
improveApprox :: LTS -> Text -> Mu.Formula -> Set CCS.Process -> Either LTS.Err (Set CCS.Process)
improveApprox lts binding formula approx = do
  -- TODO avoid inf looping

  let muEnv = Map.insert binding approx Map.empty
  verified <- verify muEnv lts formula

  transitions <- getTransitions lts -- TODO this could be cached in the LTS struct?
  vs <- Control.Monad.forM transitions $ \(_evt, lts') ->
    improveApprox lts' binding formula approx

  let base = Set.fromList [lts._process | verified]
  return $ foldr Set.union base vs

verify :: MuEnv -> LTS -> Mu.Formula -> Either LTS.Err Bool
verify muEnv lts formula = do
  let verify_ = verify muEnv lts
  transitions <- getTransitions lts
  case formula of
    Mu.Bottom -> Right False
    Mu.And l r -> do
      l' <- verify_ l
      r' <- verify_ r
      Right (l' && r')
    Mu.Atom bin -> return $ case Map.lookup bin muEnv of
      Nothing -> False
      Just s -> lts._process `Set.member` s
    Mu.Not formula' -> do
      b <- verify_ formula'
      Right $ not b
    Mu.Mu binding body -> do
      -- TODO refactor as "findFixPoint" for perf reasons
      fixPoint <- findGreatestFixpoint (improveApprox lts binding body) Set.empty
      return $ lts._process `Set.member` fixPoint
    Mu.Diamond evt formula' ->
      case evt of
        Mu.EvtAnd l r -> do
          l' <- verify_ (Mu.Diamond l formula')
          r' <- verify_ (Mu.Diamond r formula')
          Right (l' && r')
        Mu.EvtNot evt' -> do
          b <- verify_ (Mu.Diamond evt' formula')
          Right $ not b
        Mu.Up -> do
          bools <- Control.Monad.forM transitions $ \(_, lts') ->
            verify muEnv lts' formula'
          Right $ or bools
        Mu.Evt evt' -> do
          bools <- Control.Monad.forM transitions $ \(evt'', lts') -> do
            b <- verify muEnv lts' formula'
            Right $ evt' == evt'' && b
          Right $ or bools

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
verifyProgram definitions = [(def, verifyDefinitionSpecs defsMap def) | def <- definitions]
 where
  defsMap :: DefinitionsMap
  defsMap = Map.fromList [(def.name, def) | def <- definitions]

data LTS
  = State
  { _process :: CCS.Process
  , _defsMap :: DefinitionsMap
  }

verifyDefinitionSpecs :: DefinitionsMap -> CCS.Definition -> Either LTS.Err [FailingSpec]
verifyDefinitionSpecs defsMap_ def = do
  vs <- Control.Monad.forM def.specs $ \(CCS.Ranged () formula) -> do
    let lts = State{_process = def.definition, _defsMap = defsMap_}
    b <- Mu.Verify.verify Map.empty lts formula
    return ([FalsifiedFormula formula | not b])
  Right $ concat vs

findGreatestFixpoint :: (Eq t) => (t -> Either err t) -> t -> Either err t
findGreatestFixpoint f x = do
  next <- f x
  if next == x
    then return next
    else findGreatestFixpoint f next
