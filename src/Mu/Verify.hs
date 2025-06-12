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
import Data.Text (Text)
import qualified Mu.Formula as Mu

type DefinitionsMap = Map Text CCS.Definition

newtype FailingSpec
  = FalsifiedFormula Mu.Formula

verify :: LTS -> Mu.Formula -> Either LTS.Err Bool
verify lts formula = do
  transitions <- getTransitions lts
  case formula of
    Mu.Bottom -> Right False
    Mu.And l r -> do
      l' <- verify lts l
      r' <- verify lts r
      Right (l' && r')
    Mu.Atom _ -> Right False -- TODO implement for fixed point
    Mu.Not formula' -> do
      b <- verify lts formula'
      Right $ not b
    Mu.Mu _ _ -> error "TODO mu"
    Mu.Diamond evt formula' ->
      case evt of
        Mu.EvtAnd l r -> do
          l' <- verify lts (Mu.Diamond l formula')
          r' <- verify lts (Mu.Diamond r formula')
          Right (l' && r')
        Mu.EvtNot evt' -> do
          b <- verify lts (Mu.Diamond evt' formula')
          Right $ not b
        Mu.Up -> do
          bools <- Control.Monad.forM transitions $ \(_, lts') ->
            verify lts' formula'
          Right $ or bools
        Mu.Evt evt' -> do
          bools <- Control.Monad.forM transitions $ \(evt'', lts') -> do
            b <- verify lts' formula'
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
    b <- Mu.Verify.verify lts formula
    return ([FalsifiedFormula formula | not b])
  Right $ concat vs
