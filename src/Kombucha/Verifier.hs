module Kombucha.Verifier where

import Control.Arrow
import Kombucha.Inference
import qualified Kombucha.Parser as Parser
import Text.Parsec

data VerificationError
  = ParseError ParseError
  | TypeError TypeError
  deriving (Eq, Show)

verify :: String -> Either VerificationError ()
verify document = do
  document' <- left ParseError $ runParser Parser.document () "" document
  left TypeError $ checkDocument document'
