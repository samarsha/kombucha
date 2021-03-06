module Kombucha.SyntaxTree
  ( Axiom (..),
    Claim (..),
    Declaration (..),
    Document,
    Expr (..),
    Inference (..),
    Name,
    Param (..),
    ParamSpec (..),
    Pattern (..),
    Proof (..),
    Resource (..),
    ResourceSpec (..),
    Scheme (..),
    Type (..),
    inferenceScheme,
  )
where

import Data.List
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe
import Kombucha.TwoOrMore (TwoOrMore)
import qualified Kombucha.TwoOrMore as TwoOrMore

type Document = [Declaration]

data Declaration
  = DeclareParam ParamSpec
  | DeclareResource ResourceSpec
  | DeclareAxiom Axiom
  | DeclareClaim Claim
  deriving (Eq, Show)

type Name = String

data Scheme a = ForAll [Name] a
  deriving (Eq, Show)

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
    inference :: Scheme Inference
  }
  deriving (Eq, Show)

data Claim = Claim
  { name :: Name,
    inference :: Scheme Inference,
    proof :: Proof
  }
  deriving (Eq, Show)

data Inference = Resource :|- Resource
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
  deriving (Eq, Show)

inferenceScheme :: Inference -> Scheme Inference
inferenceScheme inference@(lhs :|- rhs) = ForAll (nub variables) inference
  where
    variables = resourceVariables lhs ++ resourceVariables rhs

resourceVariables :: Resource -> [Name]
resourceVariables resource = case resource of
  ResourceUnit -> []
  ResourceAtom _ params -> mapMaybe paramVariable params
  ResourceTuple resources -> TwoOrMore.toList resources >>= resourceVariables
  ResourceVariable name -> [name]

paramVariable :: Param -> Maybe Name
paramVariable (ParamVariable name) = Just name
paramVariable (ParamValue _) = Nothing
