{-# LANGUAGE OverloadedStrings #-}

module LTL.Parser (parse) where

import Control.Applicative.Combinators (choice)
import qualified Control.Monad.Combinators.Expr as Expr
import Data.Text (Text)
import Data.Void
import qualified LTL.Formula as LTL
import Parser (Parser, lexeme, lowercaseIdent, nestablePrefixes, parens, sc, symbol)
import Text.Megaparsec (
  MonadParsec (eof),
  (<?>),
 )
import qualified Text.Megaparsec

parse :: Text -> Either (Text.Megaparsec.ParseErrorBundle Text Void) LTL.Formula
parse = Text.Megaparsec.parse (sc *> formula <* eof) "ltl"

formula :: Parser LTL.Formula
formula = Expr.makeExprParser term operatorTable <?> "formula"

term :: Parser LTL.Formula
term =
  choice
    [ parens formula
    , LTL.always <$ symbol "true"
    , LTL.Bottom <$ symbol "false"
    , LTL.Atom <$> lexeme lowercaseIdent
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
