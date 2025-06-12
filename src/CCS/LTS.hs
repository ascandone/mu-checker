{-# LANGUAGE OverloadedRecordDot #-}

module CCS.LTS (
  getTransitions,
  Err (..),
) where

import qualified CCS.Program as CCS
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

data BadArityErr
  = ExtraArgs
  | MissingArgs
  deriving (Eq, Ord, Show)

data Err
  = UnboundBinding Text
  | BadArity BadArityErr (NonEmpty Text)
  deriving (Eq, Ord, Show)

-- LTS stuff

getTransitions :: Map Text CCS.Definition -> CCS.Process -> Either Err [(Maybe CCS.EventChoice, CCS.Process)]
getTransitions defs proc_ =
  case proc_ of
    CCS.Ident name args ->
      case Map.lookup name defs of
        Nothing -> Left (UnboundBinding name)
        Just defLookup -> do
          proc' <- applyParams defLookup.params args defLookup.definition
          getTransitions defs proc'
    CCS.Choice xs ->
      Right [(Just choice, proc') | (choice, proc') <- xs]
    CCS.Par l r -> do
      leftTransitions <- getTransitions defs l
      rightTransitions <- getTransitions defs r
      let handshakes = getHandshakes leftTransitions rightTransitions
      let leftSide = [(choice, CCS.Par leftProc r) | (choice, leftProc) <- leftTransitions]
      let rightSide = [(choice, CCS.Par l rightProc) | (choice, rightProc) <- rightTransitions]
      return (leftSide ++ rightSide ++ handshakes)
    CCS.Restriction label proc' -> do
      transitions <- getTransitions defs proc'
      Right [(evt, CCS.Restriction label nextProc) | tr@(evt, nextProc) <- transitions, unrestricted label tr]

getHandshakes :: [(Maybe CCS.EventChoice, CCS.Process)] -> [(Maybe CCS.EventChoice, CCS.Process)] -> [(Maybe a, CCS.Process)]
getHandshakes leftTransitions rightTransitions =
  [ (Nothing, CCS.Par lProc rProc)
  | (Just evtL, lProc) <- leftTransitions
  , (Just evtR, rProc) <- rightTransitions
  , handshake evtL evtR
  ]

handshake :: CCS.EventChoice -> CCS.EventChoice -> Bool
handshake e1 e2 = case (e1, e2) of
  (CCS.Rcv l1, CCS.Snd l2) | l1 == l2 -> True
  (CCS.Snd l1, CCS.Rcv l2) | l1 == l2 -> True
  _ -> False

unrestricted :: Text -> (Maybe CCS.EventChoice, x) -> Bool
unrestricted l (evt, _) = case evt of
  Just (CCS.Rcv e) -> e /= l
  Just (CCS.Snd e) -> e /= l
  Nothing -> True

applyParams :: [Text] -> [Text] -> CCS.Process -> Either Err CCS.Process
applyParams params args proc_ =
  case (params, args) of
    ([], []) -> return proc_
    (param : params', arg : args') ->
      applyParams params' args' (applyParam param arg proc_)
    ([], arg : args') -> Left $ BadArity ExtraArgs (arg :| args')
    (param : params', []) -> Left $ BadArity MissingArgs (param :| params')

applyParam :: Text -> Text -> CCS.Process -> CCS.Process
applyParam param arg proc_ = case proc_ of
  -- TODO apply params
  CCS.Ident name params ->
    CCS.Ident name [substitute b | b <- params]
  CCS.Restriction label proc' ->
    -- TODO not sure it makes sense to substitute the label
    CCS.Restriction (substitute label) (applyParam param arg proc')
  CCS.Choice choices ->
    CCS.Choice [(substituteEvt evt, applyParam param arg proc') | (evt, proc') <- choices]
  CCS.Par l r ->
    CCS.Par (applyParam param arg l) (applyParam param arg r)
 where
  substitute x | x == param = arg
  substitute x = x
  substituteEvt (CCS.Snd e) = CCS.Snd (substitute e)
  substituteEvt (CCS.Rcv e) = CCS.Rcv (substitute e)
