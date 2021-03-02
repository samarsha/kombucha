module Kombucha.Parser (parameterSpec, resourceSpec) where

import Kombucha.SyntaxTree
import Kombucha.TwoOrMore
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token (TokenParser)
import qualified Text.Parsec.Token as Token

tokenParser :: TokenParser ()
tokenParser =
  Token.makeTokenParser
    Token.LanguageDef
      { commentStart = "",
        commentEnd = "",
        commentLine = "",
        nestedComments = False,
        identStart = letter,
        identLetter = letter,
        opStart = parserZero,
        opLetter = parserZero,
        reservedNames = ["parameter", "resource"],
        reservedOpNames = [],
        caseSensitive = True
      }

reserved :: String -> Parser ()
reserved = Token.reserved tokenParser

identifier :: Parser String
identifier = Token.identifier tokenParser

symbol :: String -> Parser String
symbol = Token.symbol tokenParser

sepBy2 :: Parser a -> Parser sep -> Parser (TwoOrMore a)
sepBy2 p sep = do
  first <- p
  _ <- sep
  second <- p
  rest <- many $ sep >> p
  return $ TwoOrMore first second rest

parameterSpec :: Parser ParameterSpec
parameterSpec = do
  reserved "parameter"
  name <- identifier
  _ <- symbol "="
  values <- sepBy2 identifier $ symbol "|"
  return ParameterSpec {name, values}

resourceSpec :: Parser ResourceSpec
resourceSpec = do
  reserved "resource"
  name <- identifier
  parameters <- many identifier
  return ResourceSpec {name, parameters}
