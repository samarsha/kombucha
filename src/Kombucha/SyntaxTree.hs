module Kombucha.SyntaxTree where

data Declaration
  = DeclareParameter ResourceSpec
  | DeclareResource ResourceSpec
  | DeclareAxiom Axiom
  | DeclareClaim Claim

data ParameterSpec = ParameterSpec
  { name :: String,
    parameters :: [String]
  }

data Resource
  = ResourceUnit
  | ResourceAtom String [String]
  | ResourceTuple [Resource]
  | ResourceVariable Char

data ResourceSpec = ResourceSpec
  { name :: String,
    parameterTypes :: [String]
  }

data Axiom = Axiom
  { name :: String,
    inference :: Inference
  }

data Claim = Claim
  { name :: String,
    inference :: Inference,
    proof :: Proof
  }

data Inference = Inference
  { lhs :: Resource,
    rhs :: Resource
  }

data Proof = Proof
  { input :: Pattern,
    output :: Expr
  }

data Pattern
  = PatternBinding String
  | PatternTuple [Pattern]
  | PatternUnit

data Expr
  = ExprTuple [Expr]
  | ExprVariable String
  | ExprUnit
  | ExprLet
  | ExprApply String Expr
