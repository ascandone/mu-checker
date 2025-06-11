{-# LANGUAGE OverloadedStrings #-}

module Mu.Formula.Parser (parse, formulaParser) where

import Control.Applicative.Combinators (choice)
import qualified Control.Monad.Combinators.Expr as Expr
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import qualified Mu.Formula as Mu
import Parser (Parser, lexeme, nestablePrefixes, parens, sc, symbol)
import Text.Megaparsec (
  MonadParsec (eof),
  between,
  many,
  (<?>),
 )
import qualified Text.Megaparsec
import Text.Megaparsec.Char

formulaParser :: Parser Mu.Formula
formulaParser = formula

parse :: Text -> Either (Text.Megaparsec.ParseErrorBundle Text Void) Mu.Formula
parse = Text.Megaparsec.parse (sc *> formula <* eof) "ltl"

ident :: Parser Text
ident = lexeme $ do
  first <- lowerChar
  rest <- many alphaNumChar
  return $ T.pack (first : rest)

evtIdent :: Parser Mu.FormulaEvent
evtIdent = lexeme $ do
  first <- lowerChar
  rest <- many alphaNumChar
  let text = T.pack (first : rest)
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
    , Mu.Atom <$> ident
    ]
    <?> "term"

operatorTable :: [[Expr.Operator Parser Mu.Formula]]
operatorTable =
  [
    [ nestablePrefixes
        [ Mu.Not <$ symbol "!"
        , Mu.Diamond <$> diamond
        , Mu.box <$> box
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
    [ Mu.evtAlways <$ symbol "true"
    , Mu.EvtBottom <$ symbol "false"
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
