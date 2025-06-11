{-# LANGUAGE OverloadedStrings #-}

module Parser (
  lexeme,
  symbol,
  nestablePrefixes,
  sc,
  Parser,
  parens,
  lowercaseIdent,
  uppercaseIdent,
) where

import Control.Applicative.Combinators (choice)
import qualified Control.Monad.Combinators.Expr as Expr
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void
import Text.Megaparsec (Parsec, between, empty, many, some, (<|>))
import Text.Megaparsec.Char (alphaNumChar, char, lowerChar, space1, upperChar)
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
  Expr.Prefix (foldr1 (.) <$> some (choice pr))

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | Not a lexeme
lowercaseIdent :: Parser Text
lowercaseIdent = do
  first <- lowerChar
  rest <- many (alphaNumChar <|> char '_')
  return $ Text.pack (first : rest)

-- | Not a lexeme
uppercaseIdent :: Parser Text
uppercaseIdent = do
  first <- upperChar
  rest <- many (alphaNumChar <|> char '_')
  return $ Text.pack (first : rest)
