{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Mu.Verify (
  LTS (..),
  verify,
) where

import qualified Control.Monad
import qualified Mu.Formula as Mu

data LTS state label err
  = State state (Either err [(label, LTS state label err)])

verify :: (Eq state, Ord state) => LTS state Mu.Evt err -> Mu.Formula -> Either err Bool
verify lts@(State _ transitions_) formula = do
  transitions <- transitions_
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
