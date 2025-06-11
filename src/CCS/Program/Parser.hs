{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use <$" #-}

module CCS.Program.Parser (
  parse,
  parseProc,
  Text.Megaparsec.errorBundlePretty,
) where

import qualified CCS.Program as CCS
import qualified CCS.Program as LTL
import Control.Applicative.Combinators (choice)
import qualified Control.Monad.Combinators.Expr as Expr
import qualified Data.Maybe
import Data.Text (Text)
import Data.Void
import qualified Mu.Formula as Mu
import qualified Mu.Formula.Parser
import Parser (Parser, lexeme, lowercaseIdent, parens, sc, symbol, uppercaseIdent)
import Text.Megaparsec (MonadParsec (eof), between, many, optional, sepBy, sepBy1, (<?>))
import qualified Text.Megaparsec

parse :: String -> Text -> Either (Text.Megaparsec.ParseErrorBundle Text Void) CCS.Program
parse = Text.Megaparsec.parse (sc *> programP <* eof)

parseProc :: String -> Text -> Either (Text.Megaparsec.ParseErrorBundle Text Void) CCS.Process
parseProc = Text.Megaparsec.parse (sc *> processP <* eof)

ident :: Parser Text
ident = lexeme lowercaseIdent

choiceIdent :: Parser CCS.EventChoice
choiceIdent = lexeme $ do
  name <- lowercaseIdent
  choice
    [ CCS.Snd name <$ symbol "!"
    , CCS.Rcv name <$ symbol "?"
    ]

procIdent :: Parser Text
procIdent = lexeme uppercaseIdent

programP :: Parser [CCS.Definition]
programP = many definitionP

ranged :: Parser p -> Parser (CCS.Ranged p)
ranged p = CCS.Ranged () <$> p

specP :: Parser (CCS.Ranged Mu.Formula)
specP = ranged (symbol "@specs" *> Mu.Formula.Parser.formulaParser)

definitionP :: Parser CCS.Definition
definitionP =
  return CCS.Definition
    <*> many specP
    <*> procIdent
    <*> procIdentArgs
    <* symbol "="
    <*> processP

listOptional :: Parser [a] -> Parser [a]
listOptional p =
  Data.Maybe.fromMaybe [] <$> optional p

procIdentArgs :: Parser [Text]
procIdentArgs =
  listOptional $
    parens (ident `sepBy` symbol ",")

processP :: Parser LTL.Process
processP = Expr.makeExprParser procTerm operatorTable <?> "process"

ccsChoice :: Parser (CCS.EventChoice, CCS.Process)
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

procTermUnrestricted :: Parser CCS.Process
procTermUnrestricted =
  choice
    [ parens processP
    , CCS.Choice [] <$ symbol "0"
    , CCS.Choice <$> ccsChoice `sepBy1` symbol "+"
    , CCS.Ident <$> procIdent <*> procIdentArgs
    ]
    <?> "process term"

operatorTable :: [[Expr.Operator Parser CCS.Process]]
operatorTable =
  [
    [ Expr.InfixL $ CCS.Par <$ symbol "|"
    ]
  ]
