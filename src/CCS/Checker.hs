{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Move guards forward" #-}
{-# HLINT ignore "Use list comprehension" #-}

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
  let lts = makeLts defsMap def.definition
  vs <- Control.Monad.forM def.specs $ \(CCS.Ranged () formula) -> do
    b <- Mu.Verify.verify lts formula
    return $
      if b
        then []
        else [FalsifiedFormula formula]
  Right $ concat vs

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

makeLts :: DefinitionsMap -> CCS.Process -> LTS CCS.Process Mu.Evt CCS.LTS.Err
makeLts defs proc_ =
  State proc_ $ do
    transitions <- CCS.LTS.getTransitions defs proc_
    return [(mapChoice choice, makeLts defs nextProc) | (choice, nextProc) <- transitions]
