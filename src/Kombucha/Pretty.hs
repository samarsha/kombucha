module Kombucha.Pretty where

import Prettyprinter

data Syntax
  = SyntaxClaim
  | SyntaxKeyword
  | SyntaxOperator
  | SyntaxType

class PrettySyntax a where
  prettySyntax :: a -> Doc Syntax
