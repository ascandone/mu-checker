{-# LANGUAGE OverloadedStrings #-}

module LTL.Formula.Parser (parse) where

import Control.Applicative.Combinators (choice)
import qualified Control.Monad.Combinators.Expr as Expr
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import qualified LTL.Formula as LTL
import Parser (Parser, lexeme, sc, symbol)
import Text.Megaparsec (
  MonadParsec (eof),
  between,
  many,
  (<?>),
 )
import qualified Text.Megaparsec
import Text.Megaparsec.Char

parse :: Text -> Either (Text.Megaparsec.ParseErrorBundle Text Void) LTL.Formula
parse = Text.Megaparsec.parse (sc *> formula <* eof) "ltl"

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
    [ parens formula
    , LTL.always <$ symbol "true"
    , LTL.Bottom <$ symbol "false"
    , LTL.Atom <$> ident
    ]
    <?> "term"

operatorTable :: [[Expr.Operator Parser LTL.Formula]]
operatorTable =
  [
    [ nestablePrefixes
        [ LTL.finally <$ symbol "F"
        , LTL.Next <$ symbol "X"
        , LTL.globally <$ symbol "G"
        , LTL.Not <$ symbol "!"
        ]
    ]
  ,
    [ Expr.InfixR $ LTL.Until <$ symbol "U"
    ]
  ,
    [ Expr.InfixL $ LTL.And <$ symbol "&&"
    , Expr.InfixL $ LTL.lor <$ symbol "||"
    , Expr.InfixL $ LTL.imply <$ symbol "->"
    ]
  ]

nestablePrefixes :: [Parser (LTL.Formula -> LTL.Formula)] -> Expr.Operator Parser LTL.Formula
nestablePrefixes pr =
  Expr.Prefix (foldr1 (.) <$> Text.Megaparsec.some (choice pr))
