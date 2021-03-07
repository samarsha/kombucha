module Kombucha.SyntaxTree where

import Data.List.NonEmpty (NonEmpty)
import Data.Set (Set)
import Kombucha.TwoOrMore (TwoOrMore)

type Name = String

type Document = [Declaration]

-- * Declarations

data Declaration
  = DeclareParam ParamSpec
  | DeclareResource ResourceSpec
  | DeclareAxiom Axiom
  | DeclareClaim Claim
  deriving (Eq, Show)

data ParamSpec = ParamSpec
  { name :: Name,
    values :: TwoOrMore Name
  }
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

data Proof = Pattern `Proves` Expr
  deriving (Eq, Show)

-- * Patterns

data Pattern
  = PatternUnit
  | PatternBind Name
  | PatternTuple (TwoOrMore Pattern)
  deriving (Eq, Show)

-- * Expressions

data Expr
  = ExprUnit
  | ExprVariable Name
  | ExprTuple (TwoOrMore Expr)
  | ExprLet Pattern Expr
  | ExprApply Name Expr
  | ExprBlock (NonEmpty Expr)
  deriving (Eq, Show)

-- * Types

data Type
  = TypeInference Inference
  | TypeResource Resource
  | TypeParam Name
  | TypeVariable Name
  deriving (Eq, Show)

data Inference = Type :|- Type
  deriving (Eq, Show)

data Resource
  = ResourceUnit
  | ResourceAtom Name [Type]
  | ResourceTuple (TwoOrMore Type)
  deriving (Eq, Show)

data Qualified t = [Predicate] :=> t
  deriving (Eq, Show)

data Predicate
  = IsResource Type
  | IsParam Type Name
  deriving (Eq, Show)

data Scheme = ForAll (Set Name) (Qualified Type)
  deriving (Eq, Show)
