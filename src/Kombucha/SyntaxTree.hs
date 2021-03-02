module Kombucha.SyntaxTree where

import Kombucha.TwoOrMore

data Declaration
  = DeclareParameter ParameterSpec
  | DeclareResource ResourceSpec
  | DeclareAxiom Axiom
  | DeclareClaim Claim

data Parameter
  = ParamValue String
  | ParamVariable Char

data ParameterSpec = ParameterSpec
  { name :: String,
    values :: TwoOrMore String
  }
  deriving (Eq, Show)

data Resource
  = ResourceUnit
  | ResourceAtom String [Parameter]
  | ResourceTuple (TwoOrMore Resource)
  | ResourceVariable Char

data ResourceSpec = ResourceSpec
  { name :: String,
    parameters :: [String]
  }
  deriving (Eq, Show)

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
  | PatternTuple (TwoOrMore Pattern)

data Let = Let
  { pattern :: Pattern,
    value :: Expr,
    result :: Expr
  }

data Expr
  = ExprUnit
  | ExprVariable String
  | ExprTuple (TwoOrMore Expr)
  | ExprLet Let
  | ExprApply String Expr
