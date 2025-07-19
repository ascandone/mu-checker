{-# LANGUAGE OverloadedRecordDot #-}

module Mu.Checker (
  verifyProgram,
  FailingSpec (..),
) where

import qualified CCS.LTS as LTS
import qualified CCS.Program as CCS
import qualified Control.Monad
import Control.Monad.Error.Class (liftEither)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Mu.Formula as Mu
import qualified Parser

type MuEnv = Map Text (Set CCS.Process)

type DefinitionsMap = Map Text CCS.Definition

newtype FailingSpec
  = FalsifiedFormula Mu.Formula

reachableFrom :: DefinitionsMap -> CCS.Process -> Either LTS.Err [CCS.Process]
reachableFrom defsmap proc_ = reachableFromMany Set.empty [proc_]
 where
  reachableFromMany _ [] = return []
  reachableFromMany visitedProcs (hd : tl)
    | hd `Set.member` visitedProcs = reachableFromMany visitedProcs tl
  reachableFromMany visitedProcs (hd : tl) = do
    transitions <- LTS.getTransitions defsmap hd
    rest <- reachableFromMany (Set.insert hd visitedProcs) ([p | (_, p) <- transitions] ++ tl)
    return $ hd : rest

improveApprox :: DefinitionsMap -> MuEnv -> CCS.Process -> Mu.Formula -> Either LTS.Err [CCS.Process]
improveApprox defsMap muEnv proc_ formula = do
  reachableProcs <- liftEither $ reachableFrom defsMap proc_
  Control.Monad.filterM
    (\reachable -> verify defsMap muEnv reachable formula)
    reachableProcs

isInLeastFixpoint :: DefinitionsMap -> Text -> MuEnv -> CCS.Process -> Mu.Formula -> Either LTS.Err Bool
isInLeastFixpoint defsMap binding env proc_ formula = loop Set.empty
 where
  loop set = do
    procs <- improveApprox defsMap (Map.insert binding set env) proc_ formula
    let newSet = Set.fromList procs
    case () of
      _ | proc_ `elem` procs -> return True
      _ | newSet `Set.isSubsetOf` set -> return False
      _ -> loop (Set.union newSet set)

verify :: DefinitionsMap -> MuEnv -> CCS.Process -> Mu.Formula -> Either LTS.Err Bool
verify defsMap muEnv proc_ formula = do
  let verify_ = verify defsMap muEnv proc_
  transitions <- LTS.getTransitions defsMap proc_
  case formula of
    Mu.Not (Mu.Not formula') -> verify defsMap muEnv proc_ formula'
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
    Mu.Mu binding body -> isInLeastFixpoint defsMap binding muEnv proc_ body
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
            verify defsMap muEnv lts' formula'
          return $ or bools
        Mu.Evt evt' -> do
          bools <- Control.Monad.forM transitions $ \(evt'', lts') -> case mapChoice evt'' of
            -- we verify again the <> formula (not formula')
            Mu.Tau | evt' /= Mu.Tau -> verify defsMap muEnv lts' formula
            mappedEvt -> do
              b <- verify defsMap muEnv lts' formula'
              return $ evt' == mappedEvt && b
          return $ or bools

-- TODO use the same datatype to avoid casting
mapChoice :: Maybe CCS.Action -> Mu.Evt
mapChoice evt =
  case evt of
    Nothing -> Mu.Tau
    Just (CCS.Action CCS.Rcv e args) -> Mu.Rcv e args
    Just (CCS.Action CCS.Snd e args) -> Mu.Snd e args

verifyProgram :: CCS.Program -> [(CCS.Definition, Either LTS.Err [FailingSpec])]
verifyProgram definitions =
  [ (def, verifyDefinitionSpecs defsMap def)
  | def <- definitions
  ]
 where
  defsMap = Map.fromList [(def.name, def) | def <- definitions]

verifyDefinitionSpecs :: DefinitionsMap -> CCS.Definition -> Either LTS.Err [FailingSpec]
verifyDefinitionSpecs defsMap def = do
  vs <- Control.Monad.forM def.specs $ \(Parser.Ranged _ formula) -> do
    b <- Mu.Checker.verify defsMap Map.empty def.definition formula
    return ([FalsifiedFormula formula | not b])
  return $ concat vs
