module Kombucha.SyntaxTree where

import Data.List.NonEmpty (NonEmpty)
import Data.Set (Set)
import qualified Data.Set as Set
import Kombucha.TwoOrMore (TwoOrMore)
import qualified Kombucha.TwoOrMore as TwoOrMore
import Prettyprinter

type Name = String

type Document = [Declaration]

-- * Declarations

data Declaration
  = DeclareType TypeDeclaration
  | DeclareAxiom Axiom
  | DeclareClaim Claim
  deriving (Eq, Show)

data TypeDeclaration
  = DeclareParam ParamSpec
  | DeclareResource ResourceSpec
  deriving (Eq, Show)

instance Pretty TypeDeclaration where
  pretty (DeclareParam param) = pretty param
  pretty (DeclareResource resource) = pretty resource

data ParamSpec = ParamSpec
  { name :: Name,
    values :: TwoOrMore Name
  }
  deriving (Eq, Show)

instance Pretty ParamSpec where
  pretty ParamSpec {name, values} =
    "parameter"
      <+> pretty name
      <+> "="
      <+> hcat (punctuate " | " $ map pretty $ TwoOrMore.toList values)

data ResourceSpec = ResourceSpec
  { name :: Name,
    params :: [Name]
  }
  deriving (Eq, Show)

instance Pretty ResourceSpec where
  pretty ResourceSpec {name, params} = "resource" <+> pretty name <+> hsep (map pretty params)

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

instance Pretty Type where
  pretty (TypeInference inference) = pretty inference
  pretty (TypeResource resource) = pretty resource
  pretty (TypeParam param) = pretty param
  pretty (TypeVariable var) = pretty var

data Inference = Type :|- Type
  deriving (Eq, Show)

instance Pretty Inference where
  pretty (type1 :|- type2) = pretty type1 <+> "|-" <+> pretty type2

data Resource
  = ResourceUnit
  | ResourceAtom Name [Type]
  | ResourceTuple (TwoOrMore Type)
  deriving (Eq, Show)

instance Pretty Resource where
  pretty ResourceUnit = "0"
  pretty (ResourceAtom name params) = pretty name <+> hsep (map pretty params)
  pretty (ResourceTuple types) = encloseSep lparen rparen " + " $ map pretty $ TwoOrMore.toList types

data Qualified t = [Predicate] :=> t
  deriving (Eq, Show)

instance Pretty t => Pretty (Qualified t) where
  pretty (predicates :=> type') =
    "such that"
      <+> hcat (punctuate (comma <> space) $ map pretty predicates)
      <> line
      <> pretty type'

data Predicate
  = IsResource Type
  | IsParam Type Name
  deriving (Eq, Show)

instance Pretty Predicate where
  pretty (IsResource type') = pretty type' <+> "is a resource"
  pretty (IsParam type' param) = pretty type' <+> "is a" <+> pretty param

data Scheme = ForAll (Set Name) (Qualified Type)
  deriving (Eq, Show)

instance Pretty Scheme where
  pretty (ForAll vars type') = "for all " <> hsep (map pretty $ Set.toList vars) <> "," <+> pretty type'
