{-# LANGUAGE OverloadedStrings #-}

module Parser (
  lexeme,
  symbol,
  nestablePrefixes,
  sc,
  Parser,
  parens,
) where

import Control.Applicative.Combinators (choice)
import qualified Control.Monad.Combinators.Expr as Expr
import Data.Text (Text)
import Data.Void
import Text.Megaparsec (Parsec, empty)
import qualified Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "//") -- Line comments
    empty -- Block comments

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

nestablePrefixes :: [Parser (a -> a)] -> Expr.Operator Parser a
nestablePrefixes pr =
  Expr.Prefix (foldr1 (.) <$> Text.Megaparsec.some (choice pr))

parens :: Parser a -> Parser a
parens = Text.Megaparsec.between (symbol "(") (symbol ")")
