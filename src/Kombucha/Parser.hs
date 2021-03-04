module Kombucha.Parser
  ( axiom,
    claim,
    expr,
    paramSpec,
    pattern,
    resource,
    resourceSpec,
  )
where

import Control.Monad
import Data.List.NonEmpty
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
      reservedNames = ["axiom", "claim", "let", "parameter", "proof", "resource"],
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

braces :: Parser a -> Parser a
braces = Token.braces tokenParser

natural :: Parser Integer
natural = Token.natural tokenParser

variable :: Parser Variable
variable = lexeme $ do
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
  ResourceTuple <$> try (sepBy2 resourceTerm $ symbol "+")
    <|> resourceTerm

resourceTerm :: Parser Resource
resourceTerm =
  parens resource
    <|> try resourceRepeat
    <|> ResourceVariable <$> try variable
    <|> ResourceAtom <$> identifier <*> many param

resourceRepeat :: Parser Resource
resourceRepeat = do
  n <- natural
  case n of
    0 -> optional resourceTerm >> return ResourceUnit
    1 -> resourceTerm
    _ -> do
      r <- resourceTerm
      return $ ResourceTuple $ TwoOrMore r r $ replicate (fromInteger $ n - 2) r

param :: Parser Param
param =
  ParamVariable <$> try variable
    <|> ParamValue <$> identifier

parseProof :: Parser Proof
parseProof = do
  reserved "proof"
  lhs <- pattern
  symbol "->"
  rhs <- expr
  semi
  return $ lhs `Proves` rhs

pattern :: Parser Pattern
pattern =
  PatternTuple <$> try (sepBy2 patternTerm $ symbol "+")
    <|> patternTerm

patternTerm :: Parser Pattern
patternTerm =
  parens pattern
    <|> (symbol "0" >> return PatternUnit)
    <|> PatternBind <$> identifier

expr :: Parser Expr
expr =
  exprLet
    <|> ExprTuple <$> try (sepBy2 exprTerm $ symbol "+")
    <|> exprTerm

exprTerm :: Parser Expr
exprTerm =
  parens expr
    <|> braces (ExprBlock . fromList <$> sepBy1 expr semi)
    <|> (symbol "0" >> return ExprUnit)
    <|> exprApplyOrVariable

exprLet :: Parser Expr
exprLet = do
  reserved "let"
  p <- pattern
  symbol "="
  ExprLet p <$> expr

exprApplyOrVariable :: Parser Expr
exprApplyOrVariable = do
  name <- identifier
  arg <- optionMaybe exprTerm
  return $ maybe (ExprVariable name) (ExprApply name) arg
