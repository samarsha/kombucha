module Kombucha.Parser
  ( axiom,
    claim,
    parameterSpec,
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

parameterSpec :: Parser ParameterSpec
parameterSpec = do
  reserved "parameter"
  name <- identifier
  symbol "="
  values <- sepBy2 identifier $ symbol "|"
  semi
  return ParameterSpec {name, values}

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
  inference' <- Kombucha.Parser.inference
  semi
  return Axiom {name, inference = inference'}

claim :: Parser Claim
claim = do
  reserved "claim"
  name <- identifier
  symbol ":"
  inference' <- Kombucha.Parser.inference
  semi
  proof' <- Kombucha.Parser.proof
  return Claim {name, inference = inference', proof = proof'}

inference :: Parser Inference
inference = do
  lhs <- resource
  symbol "|-"
  rhs <- resource
  return Inference {lhs, rhs}

resource :: Parser Resource
resource = ResourceAtom <$> identifier <*> many parameter

parameter :: Parser Parameter
parameter = (ParamVariable <$> try singleLetter) <|> (ParamValue <$> identifier)

proof :: Parser Proof
proof = do
  reserved "proof"
  input <- patternParser
  symbol "->"
  output <- expr
  semi
  return Proof {input, output}

patternParser :: Parser Pattern
patternParser = PatternBinding <$> identifier

expr :: Parser Expr
expr = ExprVariable <$> identifier
