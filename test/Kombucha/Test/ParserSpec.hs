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

  it "parses axioms" $ do
    let axiom' = parse axiom ""

    axiom' "axiom qbit_to_ebit: qbit X Y |- ebit X Y"
      `shouldParse` Axiom
        { name = "qbit_to_ebit",
          inference =
            Inference
              { lhs = ResourceAtom "qbit" [ParamVariable 'X', ParamVariable 'Y'],
                rhs = ResourceAtom "ebit" [ParamVariable 'X', ParamVariable 'Y']
              }
        }

    axiom' "axiom flip_ebit: ebit X Y |- ebit Y X"
      `shouldParse` Axiom
        { name = "flip_ebit",
          inference =
            Inference
              { lhs = ResourceAtom "ebit" [ParamVariable 'X', ParamVariable 'Y'],
                rhs = ResourceAtom "ebit" [ParamVariable 'Y', ParamVariable 'X']
              }
        }

    axiom' `shouldFailOn` "axiom axiom: qbit X Y |- ebit X Y"
    axiom' `shouldFailOn` "axiom flip_ebit: ebit X Y"
    axiom' `shouldFailOn` "axiom flip_ebit:"
    axiom' `shouldFailOn` "axiom flip_ebit"
    axiom' `shouldFailOn` "axiom"

  it "parses claims" $ do
    let claim' = parse claim ""

    claim' "claim identity_qbit: qbit X Y |- qbit X Y\nproof q -> q"
      `shouldParse` Claim
        { name = "identity_qbit",
          inference =
            Inference
              { lhs = ResourceAtom "qbit" [ParamVariable 'X', ParamVariable 'Y'],
                rhs = ResourceAtom "qbit" [ParamVariable 'X', ParamVariable 'Y']
              },
          proof =
            Proof
              { input = PatternBinding "q",
                output = ExprVariable "q"
              }
        }

    -- TODO: Require newline.
    -- claim' `shouldFailOn` "claim identity_qbit: qbit X Y |- qbit X Y proof q -> q"

    claim' `shouldFailOn` "claim identity_qbit: qbit X Y |- qbit X Y\nproof q ->"
    claim' `shouldFailOn` "claim identity_qbit: qbit X Y |- qbit X Y\nproof q"
    claim' `shouldFailOn` "claim identity_qbit: qbit X Y |- qbit X Y\nproof"
    claim' `shouldFailOn` "claim identity_qbit: qbit X Y |- qbit X Y"
    claim' `shouldFailOn` "claim claim: qbit X Y |- qbit X Y"
    claim' `shouldFailOn` "claim proof: qbit X Y |- qbit X Y"
