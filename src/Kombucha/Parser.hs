module Kombucha.Parser
  ( axiom,
    claim,
    document,
    expr,
    paramSpec,
    pattern,
    resource,
    resourceSpec,
  )
where

import Control.Monad
import qualified Data.List.NonEmpty as NonEmpty
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

variable :: Parser Name
variable = lexeme $ do
  l <- letter
  notFollowedBy $ Token.identLetter languageDef
  return [l]

sepBy2 :: Parser a -> Parser () -> Parser (TwoOrMore a)
sepBy2 p sep = do
  first <- p
  sep
  second <- p
  rest <- many $ sep >> p
  return $ TwoOrMore first second rest

document :: Parser Document
document = do
  declarations <- many declaration
  eof
  return declarations

declaration :: Parser Declaration
declaration =
  DeclareType . DeclareParam <$> paramSpec
    <|> DeclareType . DeclareResource <$> resourceSpec
    <|> DeclareAxiom <$> axiom
    <|> DeclareClaim <$> claim

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
  params <- many identifier
  semi
  return ResourceSpec {name, params}

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
  return $ lhs :|- rhs

resource :: Parser Type
resource =
  TypeResource . ResourceTuple <$> try (sepBy2 resourceTerm $ symbol "+")
    <|> resourceTerm

resourceTerm :: Parser Type
resourceTerm =
  parens resource
    <|> try resourceRepeat
    <|> TypeVariable <$> try variable
    <|> TypeResource <$> (ResourceAtom <$> identifier <*> many param)

resourceRepeat :: Parser Type
resourceRepeat = do
  n <- natural
  case n of
    0 -> optional resourceTerm >> return (TypeResource ResourceUnit)
    1 -> resourceTerm
    _ -> do
      r <- resourceTerm
      let resources = TwoOrMore r r $ replicate (fromInteger $ n - 2) r
      return $ TypeResource $ ResourceTuple resources

param :: Parser Type
param =
  TypeVariable <$> try variable
    <|> TypeParam <$> identifier

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
    <|> braces (ExprBlock . NonEmpty.fromList <$> sepBy1 expr semi)
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
