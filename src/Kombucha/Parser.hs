module Kombucha.Parser (parameterSpec) where

import Kombucha.SyntaxTree
import Kombucha.TwoOrMore
import Text.Parsec
import Text.Parsec.String

parameterSpec :: Parser ParameterSpec
parameterSpec = do
  keyword "parameter"
  name <- identifier

  _ <- lexeme $ string "="

  first <- identifier
  _ <- lexeme $ string "|"
  second <- identifier
  rest <- many $ lexeme (string "|") >> identifier

  let values = TwoOrMore first second rest
  return ParameterSpec {name, values}

identifier :: Parser String
identifier = do
  name <- many1 alphaNum
  spaces
  return name

keyword :: String -> Parser ()
keyword name = lexeme $ do
  _ <- string name
  notFollowedBy alphaNum

lexeme :: Parser a -> Parser a
lexeme parser = do
  result <- parser
  spaces
  return result
