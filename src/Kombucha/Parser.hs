module Kombucha.Parser
  ( axiom,
    claim,
    paramSpec,
    resource,
    resourceSpec,
  )
where

import Control.Monad
import Kombucha.SyntaxTree
import Kombucha.TwoOrMore
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token (LanguageDef, TokenParser)
import qualified Text.Parsec.Token as Token

tokenParser :: TokenParser ()
tokenParser = Token.makeTokenParser languageDef

languageDef :: LanguageDef state
languageDef =
  Token.LanguageDef
    { commentStart = "",
      commentEnd = "",
      commentLine = "",
      nestedComments = False,
      identStart = letter <|> char '_',
      identLetter = alphaNum <|> char '_',
      opStart = parserZero,
      opLetter = parserZero,
      reservedNames = ["axiom", "claim", "parameter", "proof", "resource"],
      reservedOpNames = [],
      caseSensitive = True
    }

reserved :: String -> Parser ()
reserved = Token.reserved tokenParser

identifier :: Parser String
identifier = Token.identifier tokenParser

symbol :: String -> Parser ()
symbol = void . Token.symbol tokenParser

lexeme :: Parser a -> Parser a
lexeme = Token.lexeme tokenParser

semi :: Parser ()
semi = void $ Token.semi tokenParser

parens :: Parser a -> Parser a
parens = Token.parens tokenParser

singleLetter :: Parser Char
singleLetter = lexeme $ do
  l <- letter
  notFollowedBy $ Token.identLetter languageDef
  return l

sepBy2 :: Parser a -> Parser () -> Parser (TwoOrMore a)
sepBy2 p sep = do
  first <- p
  sep
  second <- p
  rest <- many $ sep >> p
  return $ TwoOrMore first second rest

paramSpec :: Parser ParamSpec
paramSpec = do
  reserved "parameter"
  name <- identifier
  symbol "="
  values <- sepBy2 identifier $ symbol "|"
  semi
  return ParamSpec {name, values}

resourceSpec :: Parser ResourceSpec
resourceSpec = do
  reserved "resource"
  name <- identifier
  parameters <- many identifier
  semi
  return ResourceSpec {name, parameters}

axiom :: Parser Axiom
axiom = do
  reserved "axiom"
  name <- identifier
  symbol ":"
  inference <- parseInference
  semi
  return Axiom {name, inference}

claim :: Parser Claim
claim = do
  reserved "claim"
  name <- identifier
  symbol ":"
  inference <- parseInference
  semi
  proof <- parseProof
  return Claim {name, inference, proof}

parseInference :: Parser Inference
parseInference = do
  lhs <- resource
  symbol "|-"
  rhs <- resource
  return $ lhs `Infers` rhs

resource :: Parser Resource
resource =
  (ResourceTuple <$> try (sepBy2 resourceTerm $ symbol "+"))
    <|> resourceTerm

resourceTerm :: Parser Resource
resourceTerm =
  (ResourceVariable <$> try singleLetter)
    <|> (ResourceAtom <$> identifier <*> many param)
    <|> parens resource

param :: Parser Param
param = (ParamVariable <$> try singleLetter) <|> (ParamValue <$> identifier)

parseProof :: Parser Proof
parseProof = do
  reserved "proof"
  lhs <- parsePattern
  symbol "->"
  rhs <- expr
  semi
  return $ lhs `Proves` rhs

parsePattern :: Parser Pattern
parsePattern = PatternBinding <$> identifier

expr :: Parser Expr
expr = ExprVariable <$> identifier
