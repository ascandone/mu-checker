{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use <$" #-}

module CCS.Parser (
  parse,
  parseProc,
  Text.Megaparsec.errorBundlePretty,
) where

import qualified CCS.Program as CCS
import Control.Applicative.Combinators (choice)
import qualified Control.Monad.Combinators.Expr as Expr
import Data.Text (Text)
import Data.Void
import qualified Mu.Formula as Mu
import qualified Mu.Parser
import Parser (Parser, Ranged, args, lexeme, lowercaseIdent, parens, ranged, sc, symbol, uppercaseIdent)
import Text.Megaparsec (MonadParsec (eof), between, many, sepBy, sepBy1, (<?>))
import qualified Text.Megaparsec

parse :: String -> Text -> Either (Text.Megaparsec.ParseErrorBundle Text Void) CCS.Program
parse = Text.Megaparsec.parse (sc *> programP <* eof)

parseProc :: String -> Text -> Either (Text.Megaparsec.ParseErrorBundle Text Void) CCS.Process
parseProc = Text.Megaparsec.parse (sc *> processP <* eof)

ident :: Parser Text
ident = lexeme lowercaseIdent

eventType :: Parser CCS.ActionType
eventType =
  choice
    [ CCS.Snd <$ symbol "!"
    , CCS.Rcv <$ symbol "?"
    ]

choiceIdent :: Parser CCS.Action
choiceIdent = lexeme $ do
  name <- lowercaseIdent
  args_ <- args ident
  type_ <- eventType
  return $ CCS.Action type_ name args_

procIdent :: Parser Text
procIdent = lexeme uppercaseIdent

programP :: Parser [CCS.Definition]
programP = many definitionP

specP :: Parser (Ranged Mu.Formula)
specP = ranged (symbol "@specs" *> Mu.Parser.formulaParser)

definitionP :: Parser CCS.Definition
definitionP =
  return CCS.Definition
    <*> many specP
    <*> procIdent
    <*> args ident
    <* symbol "="
    <*> processP

processP :: Parser CCS.Process
processP = Expr.makeExprParser procTerm operatorTable <?> "process"

ccsChoice :: Parser (CCS.Action, CCS.Process)
ccsChoice =
  return (,)
    <*> choiceIdent
    <* symbol "."
    <*> procTerm

labelsList :: Parser [Text]
labelsList =
  choice
    [ between (symbol "{") (symbol "}") $
        ident `sepBy` symbol ","
    , (: []) <$> ident
    ]

procTerm :: Parser CCS.Process
procTerm = do
  term <- procTermUnrestricted
  mLabels <-
    choice
      [ symbol "\\" *> labelsList
      , return []
      ]
  return $ foldr CCS.Restriction term mLabels

multiChoiceSugar :: Parser CCS.Process
multiChoiceSugar = do
  actions <- parens (choiceIdent `sepBy1` symbol "+")
  _ <- symbol "."
  proc_ <- procTerm
  return $ CCS.Choice [(action, proc_) | action <- actions]

procTermUnrestricted :: Parser CCS.Process
procTermUnrestricted =
  choice
    [ Text.Megaparsec.try multiChoiceSugar
    , parens processP
    , CCS.Choice [] <$ symbol "0"
    , CCS.Choice <$> ccsChoice `sepBy1` symbol "+"
    , CCS.Ident <$> procIdent <*> args ident
    ]
    <?> "process term"

operatorTable :: [[Expr.Operator Parser CCS.Process]]
operatorTable =
  [
    [ Expr.InfixL $ CCS.Par <$ symbol "|"
    ]
  ]
