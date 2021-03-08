module Kombucha.SyntaxTree where

import Data.List.NonEmpty (NonEmpty)
import Data.Set (Set)
import qualified Data.Set as Set
import Kombucha.Pretty
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

instance PrettySyntax TypeDeclaration where
  prettySyntax (DeclareParam param) = prettySyntax param
  prettySyntax (DeclareResource resource) = prettySyntax resource

data ParamSpec = ParamSpec
  { name :: Name,
    values :: TwoOrMore Name
  }
  deriving (Eq, Show)

instance PrettySyntax ParamSpec where
  prettySyntax ParamSpec {name, values} =
    annotate SyntaxKeyword "parameter"
      <+> annotate SyntaxParam (pretty name)
      <+> annotate SyntaxOperator "="
      <+> hcat (punctuate (annotate SyntaxOperator " | ") $ map pretty $ TwoOrMore.toList values)

data ResourceSpec = ResourceSpec
  { name :: Name,
    params :: [Name]
  }
  deriving (Eq, Show)

instance PrettySyntax ResourceSpec where
  prettySyntax ResourceSpec {name, params} =
    annotate SyntaxKeyword "resource"
      <+> annotate SyntaxType (pretty name)
      <+> hsep (map (annotate SyntaxParam . pretty) params)

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

instance PrettySyntax Type where
  prettySyntax (TypeInference inference) = prettySyntax inference
  prettySyntax (TypeResource resource) = prettySyntax resource
  prettySyntax (TypeParam param) = pretty param
  prettySyntax (TypeVariable var) = annotate SyntaxType $ pretty var

data Inference = Type :|- Type
  deriving (Eq, Show)

instance PrettySyntax Inference where
  prettySyntax (type1 :|- type2) = prettySyntax type1 <+> annotate SyntaxOperator "|-" <+> prettySyntax type2

data Resource
  = ResourceUnit
  | ResourceAtom Name [Type]
  | ResourceTuple (TwoOrMore Type)
  deriving (Eq, Show)

instance PrettySyntax Resource where
  prettySyntax ResourceUnit = annotate SyntaxType "0"
  prettySyntax (ResourceAtom name params) =
    annotate SyntaxType (pretty name) <+> hsep (map prettySyntax params)
  prettySyntax (ResourceTuple types) =
    encloseSep lparen rparen (annotate SyntaxOperator " + ") $
      map prettySyntax $ TwoOrMore.toList types

data Qualified t = [Predicate] :=> t
  deriving (Eq, Show)

instance PrettySyntax t => PrettySyntax (Qualified t) where
  prettySyntax (predicates :=> type') =
    annotate SyntaxKeyword "such that"
      <+> hcat (punctuate (comma <> space) $ map prettySyntax predicates)
      <> line
      <> prettySyntax type'

data Predicate
  = IsResource Type
  | IsParam Type Name
  deriving (Eq, Show)

instance PrettySyntax Predicate where
  prettySyntax (IsResource type') = prettySyntax type' <+> "is a resource"
  prettySyntax (IsParam type' param) = prettySyntax type' <+> "is a" <+> annotate SyntaxParam (pretty param)

data Scheme = ForAll (Set Name) (Qualified Type)
  deriving (Eq, Show)

instance PrettySyntax Scheme where
  prettySyntax (ForAll vars type') =
    annotate SyntaxKeyword "for all "
      <> hsep (map (annotate SyntaxType . pretty) $ Set.toList vars)
      <> "," <+> prettySyntax type'
