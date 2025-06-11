{-# LANGUAGE OverloadedStrings #-}

module LTL.Formula.Parser (parse) where

import Control.Applicative.Combinators (choice)
import qualified Control.Monad.Combinators.Expr as Expr
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import qualified LTL.Formula as LTL
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

parse :: Text -> Either (Text.Megaparsec.ParseErrorBundle Text Void) LTL.Formula
parse = Text.Megaparsec.parse (formula <* eof) "ltl"

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

ident :: Parser Text
ident = lexeme $ do
  first <- lowerChar
  rest <- many alphaNumChar
  return $ T.pack (first : rest)

formula :: Parser LTL.Formula
formula = Expr.makeExprParser term operatorTable <?> "formula"

term :: Parser LTL.Formula
term =
  choice
    [ parens term
    , LTL.always <$ symbol "true"
    , LTL.Bottom <$ symbol "false"
    , LTL.Atom <$> ident
    , formula
    ]

operatorTable :: [[Expr.Operator Parser LTL.Formula]]
operatorTable =
  [
    [ Expr.Prefix (LTL.Not <$ symbol "!")
    , Expr.Prefix (LTL.Next <$ symbol "X")
    , Expr.Prefix (LTL.finally <$ symbol "F")
    , Expr.Prefix (LTL.globally <$ symbol "G")
    ]
  ,
    [ Expr.InfixR (LTL.Until <$ symbol "U") -- TODO release
    ]
  ,
    [ Expr.InfixL (LTL.And <$ symbol "&&")
    , Expr.InfixL (LTL.lor <$ symbol "||")
    , Expr.InfixL (LTL.imply <$ symbol "->")
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
