module Kombucha.SyntaxTree where

data Declaration
  = DeclareParameter ParameterSpec
  | DeclareResource ResourceSpec
  | DeclareAxiom Axiom
  | DeclareClaim Claim

data Parameter
  = ParameterValue String
  | ParameterVariable Char

data ParameterSpec = ParameterSpec
  { name :: String,
    values :: [String]
  }

data Resource
  = ResourceUnit
  | ResourceAtom String [Parameter]
  | ResourceTuple [Resource]
  | ResourceVariable Char

data ResourceSpec = ResourceSpec
  { name :: String,
    parameters :: [String]
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
  = PatternUnit
  | PatternBinding String
  | PatternTuple [Pattern]

data Let = Let
  { pattern :: Pattern,
    value :: Expr,
    result :: Expr
  }

data Expr
  = ExprUnit
  | ExprVariable String
  | ExprTuple [Expr]
  | ExprLet Let
  | ExprApply String Expr
