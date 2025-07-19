{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use camelCase" #-}

module Mu.Checker (
  verifyProgram,
  FailingSpec (..),
) where

import qualified CCS.LTS as LTS
import qualified CCS.Program as CCS
import qualified Control.Monad
import Control.Monad.Error.Class (liftEither)
import qualified Control.Monad.State.Lazy as State
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Mu.Formula as Mu
import qualified Parser

type Cache = Map (CCS.Process, Mu.Formula) Bool

type MuEnv = Map Text (Set CCS.Process)

data State = State
  { defsMap :: DefinitionsMap
  , cache :: Cache
  }

type StateM = State.StateT State (Either LTS.Err)

type DefinitionsMap = Map Text CCS.Definition

newtype FailingSpec
  = FalsifiedFormula Mu.Formula

reachableFrom :: Map Text CCS.Definition -> CCS.Process -> Either LTS.Err [CCS.Process]
reachableFrom defsmap proc_ = reachableFromMany Set.empty [proc_]
 where
  reachableFromMany _ [] = return []
  reachableFromMany visitedProcs (hd : tl)
    | hd `Set.member` visitedProcs = reachableFromMany visitedProcs tl
  reachableFromMany visitedProcs (hd : tl) = do
    transitions <- LTS.getTransitions defsmap hd
    rest <- reachableFromMany (Set.insert hd visitedProcs) ([p | (_, p) <- transitions] ++ tl)
    return $ hd : rest

improveApprox :: MuEnv -> CCS.Process -> Mu.Formula -> StateM [CCS.Process]
improveApprox muEnv proc_ formula = do
  defsMap_ <- State.gets defsMap
  reachableProcs <- liftEither $ reachableFrom defsMap_ proc_
  Control.Monad.filterM
    (\reachable -> verify muEnv reachable formula)
    reachableProcs

withEmptyCache :: StateM a -> StateM a
withEmptyCache f = do
  oldCache <- State.gets cache
  State.modify $ \st -> st{cache = Map.empty}
  b <- f
  State.modify $ \st -> st{cache = oldCache}
  return b

isInLeastFixpoint :: Text -> MuEnv -> CCS.Process -> Mu.Formula -> StateM Bool
isInLeastFixpoint binding env proc_ formula = loop Set.empty
 where
  loop set = withEmptyCache $ do
    let env' = Map.insert binding set env
    procs <- improveApprox env' proc_ formula
    case () of
      () | proc_ `elem` procs -> return True
      () | all (`elem` set) procs -> return False
      () -> loop (foldr Set.insert set procs)

verify :: MuEnv -> CCS.Process -> Mu.Formula -> StateM Bool
verify env proc_ formula = do
  cacheValue <- State.gets cache
  case Map.lookup (proc_, formula) cacheValue of
    Just b -> return b
    Nothing -> do
      value <- verify__raw env proc_ formula
      State.modify $ \st -> st{cache = Map.insert (proc_, formula) value st.cache}
      return value

verify__raw :: MuEnv -> CCS.Process -> Mu.Formula -> StateM Bool
verify__raw muEnv proc_ formula = do
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
    Mu.Mu binding body -> isInLeastFixpoint binding muEnv proc_ body
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
mapChoice :: Maybe CCS.Action -> Mu.Evt
mapChoice evt =
  case evt of
    Nothing -> Mu.Tau
    Just (CCS.Action CCS.Rcv e args) -> Mu.Rcv e args
    Just (CCS.Action CCS.Snd e args) -> Mu.Snd e args

verifyProgram :: CCS.Program -> [(CCS.Definition, Either LTS.Err [FailingSpec])]
verifyProgram definitions =
  [ (def, fst <$> State.runStateT (verifyDefinitionSpecs def) initialState)
  | def <- definitions
  ]
 where
  initialState =
    State
      { defsMap = Map.fromList [(def.name, def) | def <- definitions]
      , cache = Map.empty
      }

verifyDefinitionSpecs :: CCS.Definition -> StateM [FailingSpec]
verifyDefinitionSpecs def = do
  vs <- Control.Monad.forM def.specs $ \(Parser.Ranged _ formula) -> do
    b <- Mu.Checker.verify Map.empty def.definition formula
    return ([FalsifiedFormula formula | not b])
  return $ concat vs
