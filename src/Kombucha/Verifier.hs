module Kombucha.Verifier where

import Control.Arrow
import Data.Map (Map)
import Kombucha.Inference
import qualified Kombucha.Parser as Parser
import Kombucha.SyntaxTree
import Text.Parsec

data VerificationError
  = ParseError ParseError
  | TypeError TypeError
  deriving (Eq, Show)

verify :: String -> Either VerificationError (Map Name [Predicate])
verify document = do
  document' <- left ParseError $ runParser Parser.document () "" document
  left TypeError $ checkDocument document'
