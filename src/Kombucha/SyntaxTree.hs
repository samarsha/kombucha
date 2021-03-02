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
  deriving (Eq, Show)

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
  deriving (Eq, Show)

data ResourceSpec = ResourceSpec
  { name :: String,
    parameters :: [String]
  }
  deriving (Eq, Show)

data Axiom = Axiom
  { name :: String,
    inference :: Inference
  }
  deriving (Eq, Show)

data Claim = Claim
  { name :: String,
    inference :: Inference,
    proof :: Proof
  }
  deriving (Eq, Show)

data Inference = Inference
  { lhs :: Resource,
    rhs :: Resource
  }
  deriving (Eq, Show)

data Proof = Proof
  { input :: Pattern,
    output :: Expr
  }
  deriving (Eq, Show)

data Pattern
  = PatternUnit
  | PatternBinding String
  | PatternTuple (TwoOrMore Pattern)
  deriving (Eq, Show)

data Let = Let
  { pattern :: Pattern,
    value :: Expr,
    result :: Expr
  }
  deriving (Eq, Show)

data Expr
  = ExprUnit
  | ExprVariable String
  | ExprTuple (TwoOrMore Expr)
  | ExprLet Let
  | ExprApply String Expr
  deriving (Eq, Show)
