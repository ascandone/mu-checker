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
    [ parens term
    , Mu.always <$ symbol "true"
    , Mu.Bottom <$ symbol "false"
    , Mu.Atom <$> ident
    , formula
    ]

operatorTable :: [[Expr.Operator Parser Mu.Formula]]
operatorTable =
  [
    [ Expr.Prefix (Mu.Not <$ symbol "!")
    -- , Expr.Prefix (Mu.Next <$ symbol "X")
    -- , Expr.Prefix (Mu.finally <$ symbol "F")
    -- , Expr.Prefix (Mu.globally <$ symbol "G")
    ]
  ,
    [ Expr.InfixL (Mu.And <$ symbol "&&")
    , Expr.InfixL (Mu.lor <$ symbol "||")
    , Expr.InfixL (Mu.imply <$ symbol "->")
    ]
  ]

-- Boilerplate

type Parser = Parsec Void Text

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
