module Kombucha.SyntaxTree where

import Data.List.NonEmpty
import Kombucha.TwoOrMore

data Declaration
  = DeclareParam ParamSpec
  | DeclareResource ResourceSpec
  | DeclareAxiom Axiom
  | DeclareClaim Claim

type Variable = Char

data Param
  = ParamValue String
  | ParamVariable Variable
  deriving (Eq, Show)

data ParamSpec = ParamSpec
  { name :: String,
    values :: TwoOrMore String
  }
  deriving (Eq, Show)

data Resource
  = ResourceUnit
  | ResourceAtom String [Param]
  | ResourceTuple (TwoOrMore Resource)
  | ResourceVariable Variable
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

data Inference = Resource `Infers` Resource
  deriving (Eq, Show)

data Proof = Pattern `Proves` Expr
  deriving (Eq, Show)

data Pattern
  = PatternUnit
  | PatternBind String
  | PatternTuple (TwoOrMore Pattern)
  deriving (Eq, Show)

data Expr
  = ExprUnit
  | ExprVariable String
  | ExprTuple (TwoOrMore Expr)
  | ExprLet Pattern Expr
  | ExprApply String Expr
  | ExprBlock (NonEmpty Expr)
  deriving (Eq, Show)

data Type
  = TypeInference Inference
  | TypeResource Resource
  | TypeParam Param
  | TypeVariable Variable
