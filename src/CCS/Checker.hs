{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Move guards forward" #-}

module CCS.Checker (
  verifyProgram,
  FailingSpec (..),
) where

import qualified CCS.LTS
import qualified CCS.Program as CCS
import qualified Control.Monad
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Mu.Formula as Mu
import Mu.Verify (LTS (..))
import qualified Mu.Verify

newtype FailingSpec
  = FalsifiedFormula Mu.Formula

type DefinitionsMap = Map Text CCS.Definition

verifyDefinitionSpecs :: DefinitionsMap -> CCS.Definition -> Either CCS.LTS.Err [FailingSpec]
verifyDefinitionSpecs defsMap def = do
  lts <- makeLts defsMap def.definition
  Right [FalsifiedFormula formula | (CCS.Ranged () formula) <- def.specs, not $ Mu.Verify.verify lts formula]

verifyProgram :: CCS.Program -> [(CCS.Definition, Either CCS.LTS.Err [FailingSpec])]
verifyProgram definitions = [(def, verifyDefinitionSpecs defsMap def) | def <- definitions]
 where
  defsMap :: DefinitionsMap
  defsMap = Map.fromList [(def.name, def) | def <- definitions]

mapChoice :: Maybe CCS.EventChoice -> Mu.Evt
mapChoice evt =
  case evt of
    Nothing -> Mu.Tau
    Just (CCS.Rcv e) -> Mu.Rcv e
    Just (CCS.Snd e) -> Mu.Snd e

makeLts :: DefinitionsMap -> CCS.Process -> Either CCS.LTS.Err (LTS CCS.Process Mu.Evt)
makeLts defs proc_ = do
  transitions <- CCS.LTS.getTransitions defs proc_
  ltsTransitions <- Control.Monad.forM transitions $ \(choice, nextProc) -> do
    lts' <- makeLts defs nextProc
    return (mapChoice choice, lts')
  return $ State proc_ ltsTransitions
