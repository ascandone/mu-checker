{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module CCS.Program.Parser (parse) where

import qualified CCS.Program as CCS
import Control.Applicative.Combinators (choice)
import qualified Control.Monad.Combinators.Expr as Expr
import qualified Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec (MonadParsec (eof), Parsec, between, empty, many, optional, sepBy)
import qualified Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

parse :: Text -> Either (Text.Megaparsec.ParseErrorBundle Text Void) CCS.Program
parse = Text.Megaparsec.parse (sc *> programP <* eof) "ccs program"

argIdent :: Parser Text
argIdent = lexeme $ do
  first <- lowerChar
  rest <- many alphaNumChar
  return $ T.pack (first : rest)

procIdent :: Parser Text
procIdent = lexeme $ do
  first <- upperChar
  rest <- many alphaNumChar
  return $ T.pack (first : rest)

programP :: Parser [CCS.Definition]
programP = many definitionP

definitionP :: Parser CCS.Definition
definitionP = do
  name <- procIdent
  params <- procIdentArgs
  _ <- symbol "="
  process <- procP
  return $
    CCS.Definition
      { CCS.name = name
      , CCS.params = params
      , CCS.definition = process
      , CCS.specs = []
      }

listOptional :: Parser [a] -> Parser [a]
listOptional p =
  Data.Maybe.fromMaybe [] <$> optional p

procIdentArgs :: Parser [Text]
procIdentArgs =
  listOptional $
    lexeme $
      between "(" ")" (argIdent `sepBy` symbol ",")

procP :: Parser CCS.Process
procP =
  choice
    [ CCS.Choice [] <$ symbol "0"
    , CCS.Ident <$> procIdent <*> procIdentArgs
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
