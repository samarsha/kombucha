module Kombucha.Test.ParserSpec where

import Kombucha.Parser
import Kombucha.SyntaxTree
import Kombucha.TwoOrMore
import Test.Hspec
import Text.Parsec

spec :: Spec
spec = describe "parser" $ do
  it "parses parameter declarations" $ do
    parse parameterSpec "" "parameter Party = Alice | Bob"
      `shouldBe` Right ParameterSpec {name = "Party", values = ("Alice", "Bob") ::| []}
