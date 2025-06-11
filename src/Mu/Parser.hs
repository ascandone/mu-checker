{-# LANGUAGE OverloadedStrings #-}

module Mu.Parser (parse, formulaParser) where

import Control.Applicative.Combinators (choice)
import qualified Control.Monad.Combinators.Expr as Expr
import Data.Text (Text)
import Data.Void
import qualified Mu.Formula as Mu
import Parser (Parser, lexeme, lowercaseIdent, nestablePrefixes, parens, sc, symbol)
import Text.Megaparsec (
  MonadParsec (eof),
  between,
  (<?>),
 )
import qualified Text.Megaparsec

formulaParser :: Parser Mu.Formula
formulaParser = formula

parse :: Text -> Either (Text.Megaparsec.ParseErrorBundle Text Void) Mu.Formula
parse = Text.Megaparsec.parse (sc *> formula <* eof) "ltl"

evtIdent :: Parser Mu.FormulaEvent
evtIdent = lexeme $ do
  text <- lowercaseIdent
  choice
    [ Mu.Evt (Mu.Snd text) <$ symbol "!"
    , Mu.Evt (Mu.Rcv text) <$ symbol "?"
    ]

formula :: Parser Mu.Formula
formula = Expr.makeExprParser term operatorTable <?> "formula"

term :: Parser Mu.Formula
term =
  choice
    [ parens formula
    , Mu.always <$ symbol "true"
    , Mu.Bottom <$ symbol "false"
    , Mu.Atom <$> lexeme lowercaseIdent
    ]
    <?> "term"

operatorTable :: [[Expr.Operator Parser Mu.Formula]]
operatorTable =
  [
    [ nestablePrefixes
        [ Mu.Not <$ symbol "!"
        , Mu.Diamond <$> diamond
        , Mu.box <$> box
        , Mu.Mu <$ symbol "mu" <*> lexeme lowercaseIdent <* symbol "."
        , Mu.nu <$ symbol "nu" <*> lexeme lowercaseIdent <* symbol "."
        ]
    ]
  ,
    [ Expr.InfixL $ Mu.And <$ symbol "&&"
    , Expr.InfixL $ Mu.lor <$ symbol "||"
    , Expr.InfixL $ Mu.imply <$ symbol "->"
    ]
  ]

-- Evt

eventFormula :: Parser Mu.FormulaEvent
eventFormula = Expr.makeExprParser eventFormulaTerm evtFormulaOperatorTable

eventFormulaTerm :: Parser Mu.FormulaEvent
eventFormulaTerm =
  choice
    [ Mu.Up <$ symbol "true"
    , Mu.evtBottom <$ symbol "false"
    , Mu.Evt Mu.Tau <$ symbol "tau"
    , evtIdent
    ]
    <?> "event formula"

diamond :: Parser Mu.FormulaEvent
diamond = lexeme $ between "<" ">" eventFormula

box :: Parser Mu.FormulaEvent
box = lexeme $ between "[" "]" eventFormula

evtFormulaOperatorTable :: [[Expr.Operator Parser Mu.FormulaEvent]]
evtFormulaOperatorTable =
  [
    [ nestablePrefixes
        [ Mu.EvtNot <$ symbol "!"
        ]
    ]
  ,
    [ Expr.InfixL $ Mu.EvtAnd <$ symbol "&&"
    , Expr.InfixL $ Mu.evtOr <$ symbol "||"
    ]
  ]
