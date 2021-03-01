module Kombucha.Test.ParserSpec where

import Kombucha.Parser
import Kombucha.SyntaxTree
import Kombucha.TwoOrMore
import Test.Hspec
import Test.Hspec.Parsec
import Text.Parsec

spec :: Spec
spec = describe "parser" $ do
  it "parses parameter declarations" $ do
    let parameterSpec' = parse parameterSpec ""

    parameterSpec' "parameter Party = Alice | Bob"
      `shouldParse` ParameterSpec
        { name = "Party",
          values = ("Alice", "Bob") ::| []
        }
