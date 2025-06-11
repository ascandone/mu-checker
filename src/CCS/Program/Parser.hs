{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module CCS.Program.Parser (parse) where

import qualified CCS.Program as CCS
import qualified CCS.Program as LTL
import Control.Applicative.Combinators (choice)
import qualified Control.Monad.Combinators.Expr as Expr
import qualified Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec (MonadParsec (eof), Parsec, between, empty, many, optional, sepBy, sepBy1, (<?>))
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

choiceIdent :: Parser CCS.EventChoice
choiceIdent = lexeme $ do
  first <- lowerChar
  rest <- many alphaNumChar
  let text = T.pack (first : rest)
  choice
    [ CCS.Snd text <$ symbol "!"
    , CCS.Rcv text <$ symbol "?"
    ]

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
  process <- processP
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

processP :: Parser LTL.Process
processP = Expr.makeExprParser procTerm operatorTable <?> "process"

ccsChoice :: Parser (CCS.EventChoice, CCS.Process)
ccsChoice = do
  evt <- choiceIdent
  _ <- symbol "."
  p <- procTerm
  return (evt, p)

choicesP :: Parser [(CCS.EventChoice, CCS.Process)]
choicesP = ccsChoice `sepBy1` symbol "+"

procTerm :: Parser CCS.Process
procTerm =
  choice
    [ between "(" ")" processP
    , CCS.Choice [] <$ symbol "0"
    , CCS.Choice <$> choicesP
    , CCS.Ident <$> procIdent <*> procIdentArgs
    ]
    <?> "process term"

operatorTable :: [[Expr.Operator Parser CCS.Process]]
operatorTable =
  [
    [ Expr.InfixL $ CCS.Par <$ symbol "|"
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
