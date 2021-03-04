module Kombucha.SyntaxTree where

import Data.List.NonEmpty
import Kombucha.TwoOrMore

type Document = [Declaration]

data Declaration
  = DeclareParam ParamSpec
  | DeclareResource ResourceSpec
  | DeclareAxiom Axiom
  | DeclareClaim Claim
  deriving (Eq, Show)

type Name = String

data Param
  = ParamValue Name
  | ParamVariable Name
  deriving (Eq, Show)

data ParamSpec = ParamSpec
  { name :: Name,
    values :: TwoOrMore Name
  }
  deriving (Eq, Show)

data Resource
  = ResourceUnit
  | ResourceAtom Name [Param]
  | ResourceTuple (TwoOrMore Resource)
  | ResourceVariable Name
  deriving (Eq, Show)

data ResourceSpec = ResourceSpec
  { name :: Name,
    parameters :: [Name]
  }
  deriving (Eq, Show)

data Axiom = Axiom
  { name :: Name,
    inference :: Inference
  }
  deriving (Eq, Show)

data Claim = Claim
  { name :: Name,
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
  | PatternBind Name
  | PatternTuple (TwoOrMore Pattern)
  deriving (Eq, Show)

data Expr
  = ExprUnit
  | ExprVariable Name
  | ExprTuple (TwoOrMore Expr)
  | ExprLet Pattern Expr
  | ExprApply Name Expr
  | ExprBlock (NonEmpty Expr)
  deriving (Eq, Show)

data Type
  = TypeInference Inference
  | TypeResource Resource
  | TypeParam Param
  | TypeVariable Name
