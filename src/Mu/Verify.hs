module Mu.Verify (
  LTS (..),
  verify,
) where

import qualified Mu.Formula as Mu

data LTS state label
  = State state [(label, LTS state label)]

verify :: (Eq state, Ord state) => LTS state Mu.Evt -> Mu.Formula -> Bool
verify lts@(State _ transitions) formula = case formula of
  Mu.Bottom -> False
  Mu.And l r -> verify lts l && verify lts r
  Mu.Atom _ -> False -- TODO implement for fixed point
  Mu.Not formula' -> not (verify lts formula')
  Mu.Mu _ _ -> error "TODO mu"
  Mu.Nu _ _ -> error "TODO nu"
  Mu.Diamond evt formula' ->
    case evt of
      Mu.EvtAnd l r -> verify lts (Mu.Diamond l formula') && verify lts (Mu.Diamond r formula')
      Mu.EvtNot evt' -> not $ verify lts (Mu.Diamond evt' formula')
      Mu.EvtBottom -> False
      Mu.Evt evt' ->
        any
          (\(evt'', lts') -> evt' == evt'' && verify lts' formula')
          transitions
