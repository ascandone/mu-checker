{-# LANGUAGE OverloadedStrings #-}

module CCS.Pretty (pretty) where

import qualified CCS.Program as CCS
import Data.Text (Text)
import qualified Data.Text as T

-- TODO parens for prec
pretty :: CCS.Process -> Text
pretty proc_ = case proc_ of
  CCS.Ident name args -> case args of
    [] -> name
    _ -> name <> parens (T.intercalate ", " args)
  CCS.Choice [] -> "0"
  CCS.Choice choices -> T.intercalate " + " [prettyEvt evt <> "." <> pretty_ proc' | (evt, proc') <- choices]
  CCS.Par l r -> pretty_ l <> " | " <> pretty_ r
  CCS.Restriction _ _ ->
    case collectNestedLabels proc_ of
      (lastProc, [label]) -> pretty_ lastProc <> "\\" <> label
      (lastProc, otherLabels) -> pretty_ lastProc <> "\\{" <> T.intercalate ", " otherLabels <> "}"
 where
  pretty_ = parensPretty $ prec proc_

collectNestedLabels :: CCS.Process -> (CCS.Process, [Text])
collectNestedLabels proc_ = case proc_ of
  CCS.Restriction l body ->
    let (lastProc, otherLabels) = collectNestedLabels body
     in (lastProc, l : otherLabels)
  _ -> (proc_, [])

parensPretty :: Int -> CCS.Process -> Text
parensPretty outerPrec proc_ | prec proc_ >= outerPrec = pretty proc_
parensPretty _ proc_ = parens $ pretty proc_

prettyEvt :: CCS.Action -> Text
prettyEvt (CCS.Action t label) =
  label <> case t of
    CCS.Rcv -> "?"
    CCS.Snd -> "!"

parens :: Text -> Text
parens x = "(" <> x <> ")"

prec :: CCS.Process -> Int
prec proc_ = case proc_ of
  -- Atoms have highest prec
  CCS.Choice [] -> 99
  CCS.Ident _ _ -> 99
  -- complex exprs:
  CCS.Par _ _ -> 0
  CCS.Choice _ -> 1
  CCS.Restriction _ _ -> 2
