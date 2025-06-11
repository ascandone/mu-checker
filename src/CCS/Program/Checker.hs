{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Move guards forward" #-}

module CCS.Program.Checker (
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

data FailingSpec
  = FalsifiedFormula Mu.Formula
  | TransitionError CCS.LTS.Err
  deriving (Show)

type Definitions = Map Text CCS.Definition

verifyProgram :: CCS.Program -> [FailingSpec]
verifyProgram definitions =
  [ failingSpec
  | def <- definitions
  , (CCS.Ranged () formula) <- def.specs
  , failingSpec <- case makeLts defsMap def.definition of
      Left e -> [TransitionError e]
      Right lts -> [FalsifiedFormula formula | not (Mu.Verify.verify lts formula)]
  ]
 where
  defsMap :: Definitions
  defsMap = Map.fromList [(def.name, def) | def <- definitions]

mapChoice :: Maybe CCS.EventChoice -> Mu.Evt
mapChoice evt =
  case evt of
    Nothing -> Mu.Tau
    Just (CCS.Rcv e) -> Mu.Snd e
    Just (CCS.Snd e) -> Mu.Rcv e

makeLts :: Definitions -> CCS.Process -> Either CCS.LTS.Err (LTS CCS.Process Mu.Evt)
makeLts defs proc_ = do
  transitions <- CCS.LTS.getTransitions defs proc_
  ltsTransitions <- Control.Monad.forM transitions $ \(choice, nextProc) -> do
    lts' <- makeLts defs nextProc
    return (mapChoice choice, lts')
  return $ State proc_ ltsTransitions
