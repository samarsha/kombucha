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
          values = TwoOrMore "Alice" "Bob" []
        }

    parameterSpec' "parameter Party = Alice | Bob | Charlie"
      `shouldParse` ParameterSpec
        { name = "Party",
          values = TwoOrMore "Alice" "Bob" ["Charlie"]
        }

    parameterSpec' `shouldFailOn` "parameter parameter = Alice | Bob"
    parameterSpec' `shouldFailOn` "parameter Party = Alice | Bob |"
    parameterSpec' `shouldFailOn` "parameter Party = Alice |"
    parameterSpec' `shouldFailOn` "parameter Party = Alice"
    parameterSpec' `shouldFailOn` "parameter Party ="
    parameterSpec' `shouldFailOn` "parameter Party"

  it "parses resource declarations" $ do
    let resourceSpec' = parse resourceSpec ""

    resourceSpec' "resource qbit"
      `shouldParse` ResourceSpec
        { name = "qbit",
          parameters = []
        }

    resourceSpec' "resource qbit Party"
      `shouldParse` ResourceSpec
        { name = "qbit",
          parameters = ["Party"]
        }

    resourceSpec' "resource qbit Party Party"
      `shouldParse` ResourceSpec
        { name = "qbit",
          parameters = ["Party", "Party"]
        }

    resourceSpec' `shouldFailOn` "resource resource"
    resourceSpec' `shouldFailOn` "resource"
