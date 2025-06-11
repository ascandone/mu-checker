{-# LANGUAGE OverloadedStrings #-}

module Mu.Formula.Parser (parse) where

import Control.Applicative.Combinators (choice)
import qualified Control.Monad.Combinators.Expr as Expr
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import qualified Mu.Formula as Mu
import Text.Megaparsec (
  MonadParsec (eof),
  Parsec,
  between,
  empty,
  many,
  (<?>),
 )
import qualified Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

parse :: Text -> Either (Text.Megaparsec.ParseErrorBundle Text Void) Mu.Formula
parse = Text.Megaparsec.parse (formula <* eof) "ltl"

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

ident :: Parser Text
ident = lexeme $ do
  first <- lowerChar
  rest <- many alphaNumChar
  return $ T.pack (first : rest)

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
    -- TODO evt!, evt?
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

-- Boilerplate

type Parser = Parsec Void Text
nestablePrefixes :: [Parser (a -> a)] -> Expr.Operator Parser a
nestablePrefixes pr =
  Expr.Prefix (foldr1 (.) <$> Text.Megaparsec.some (choice pr))

sc :: Parser ()
sc =
  L.space
    space1
    empty -- Line comments
    empty -- Block comments

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc
