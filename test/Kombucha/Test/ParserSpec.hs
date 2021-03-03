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
    let paramSpec' = parse paramSpec ""

    paramSpec' "parameter Party = Alice | Bob;"
      `shouldParse` ParamSpec
        { name = "Party",
          values = TwoOrMore "Alice" "Bob" []
        }

    paramSpec' "parameter Party = Alice | Bob | Charlie;"
      `shouldParse` ParamSpec
        { name = "Party",
          values = TwoOrMore "Alice" "Bob" ["Charlie"]
        }

    paramSpec' `shouldFailOn` "parameter parameter = Alice | Bob;"
    paramSpec' `shouldFailOn` "parameter Party = Alice | Bob |"
    paramSpec' `shouldFailOn` "parameter Party = Alice | Bob"
    paramSpec' `shouldFailOn` "parameter Party = Alice |"
    paramSpec' `shouldFailOn` "parameter Party = Alice;"
    paramSpec' `shouldFailOn` "parameter Party ="
    paramSpec' `shouldFailOn` "parameter Party;"

  it "parses resource declarations" $ do
    let resourceSpec' = parse resourceSpec ""

    resourceSpec' "resource qbit;"
      `shouldParse` ResourceSpec
        { name = "qbit",
          parameters = []
        }

    resourceSpec' "resource qbit Party;"
      `shouldParse` ResourceSpec
        { name = "qbit",
          parameters = ["Party"]
        }

    resourceSpec' "resource qbit Party Party;"
      `shouldParse` ResourceSpec
        { name = "qbit",
          parameters = ["Party", "Party"]
        }

    resourceSpec' `shouldFailOn` "resource resource;"
    resourceSpec' `shouldFailOn` "resource;"

  it "parses resources" $ do
    let resource' = parse resource ""

    resource' "A" `shouldParse` ResourceVariable 'A'
    resource' "a" `shouldParse` ResourceVariable 'a'

    resource' "qbit" `shouldParse` ResourceAtom "qbit" []
    resource' "qbit X" `shouldParse` ResourceAtom "qbit" [ParamVariable 'X']
    resource' "qbit X Y" `shouldParse` ResourceAtom "qbit" [ParamVariable 'X', ParamVariable 'Y']
    resource' "qbit Alice Y" `shouldParse` ResourceAtom "qbit" [ParamValue "Alice", ParamVariable 'Y']
    resource' "qbit Alice Bob" `shouldParse` ResourceAtom "qbit" [ParamValue "Alice", ParamValue "Bob"]

    resource' "A + B" `shouldParse` ResourceTuple (TwoOrMore (ResourceVariable 'A') (ResourceVariable 'B') [])
    resource' "A + B" `shouldParse` ResourceTuple (TwoOrMore (ResourceVariable 'A') (ResourceVariable 'B') [])

    resource' "A + B + C"
      `shouldParse` ResourceTuple (TwoOrMore (ResourceVariable 'A') (ResourceVariable 'B') [ResourceVariable 'C'])

    resource' "(A + B) + C"
      `shouldParse` ResourceTuple
        ( TwoOrMore
            (ResourceTuple $ TwoOrMore (ResourceVariable 'A') (ResourceVariable 'B') [])
            (ResourceVariable 'C')
            []
        )

    resource' "(qbit X Y + qbit X Y) + qbit X Y"
      `shouldParse` ResourceTuple
        ( TwoOrMore
            ( ResourceTuple $
                TwoOrMore
                  (ResourceAtom "qbit" [ParamVariable 'X', ParamVariable 'Y'])
                  (ResourceAtom "qbit" [ParamVariable 'X', ParamVariable 'Y'])
                  []
            )
            (ResourceAtom "qbit" [ParamVariable 'X', ParamVariable 'Y'])
            []
        )

    resource' "5 A"
      `shouldParse` ResourceTuple
        ( TwoOrMore
            (ResourceVariable 'A')
            (ResourceVariable 'A')
            [ResourceVariable 'A', ResourceVariable 'A', ResourceVariable 'A']
        )

    resource' "2 qbit X Y"
      `shouldParse` ResourceTuple
        ( TwoOrMore
            (ResourceAtom "qbit" [ParamVariable 'X', ParamVariable 'Y'])
            (ResourceAtom "qbit" [ParamVariable 'X', ParamVariable 'Y'])
            []
        )

    resource' "2 A + 3 A"
      `shouldParse` ResourceTuple
        ( TwoOrMore
            (ResourceTuple (TwoOrMore (ResourceVariable 'A') (ResourceVariable 'A') []))
            (ResourceTuple (TwoOrMore (ResourceVariable 'A') (ResourceVariable 'A') [ResourceVariable 'A']))
            []
        )

    resource' "2 qbit X Y + 3 qbit X Y"
      `shouldParse` ResourceTuple
        ( TwoOrMore
            ( ResourceTuple
                ( TwoOrMore
                    (ResourceAtom "qbit" [ParamVariable 'X', ParamVariable 'Y'])
                    (ResourceAtom "qbit" [ParamVariable 'X', ParamVariable 'Y'])
                    []
                )
            )
            ( ResourceTuple
                ( TwoOrMore
                    (ResourceAtom "qbit" [ParamVariable 'X', ParamVariable 'Y'])
                    (ResourceAtom "qbit" [ParamVariable 'X', ParamVariable 'Y'])
                    [ResourceAtom "qbit" [ParamVariable 'X', ParamVariable 'Y']]
                )
            )
            []
        )

  it "parses axioms" $ do
    let axiom' = parse axiom ""

    axiom' "axiom qbit_to_ebit: qbit X Y |- ebit X Y;"
      `shouldParse` Axiom
        { name = "qbit_to_ebit",
          inference =
            ResourceAtom "qbit" [ParamVariable 'X', ParamVariable 'Y']
              `Infers` ResourceAtom "ebit" [ParamVariable 'X', ParamVariable 'Y']
        }

    axiom' "axiom flip_ebit: ebit X Y |- ebit Y X;"
      `shouldParse` Axiom
        { name = "flip_ebit",
          inference =
            ResourceAtom "ebit" [ParamVariable 'X', ParamVariable 'Y']
              `Infers` ResourceAtom "ebit" [ParamVariable 'Y', ParamVariable 'X']
        }

    axiom' `shouldFailOn` "axiom qbit_to_ebit: qbit X Y |- ebit X Y"
    axiom' `shouldFailOn` "axiom axiom: qbit X Y |- ebit X Y;"
    axiom' `shouldFailOn` "axiom flip_ebit: ebit X Y;"
    axiom' `shouldFailOn` "axiom flip_ebit:"
    axiom' `shouldFailOn` "axiom flip_ebit;"
    axiom' `shouldFailOn` "axiom;"

  it "parses claims" $ do
    let claim' = parse claim ""

    claim' "claim identity_qbit: qbit X Y |- qbit X Y; proof q -> q;"
      `shouldParse` Claim
        { name = "identity_qbit",
          inference =
            ResourceAtom "qbit" [ParamVariable 'X', ParamVariable 'Y']
              `Infers` ResourceAtom "qbit" [ParamVariable 'X', ParamVariable 'Y'],
          proof = PatternBinding "q" `Proves` ExprVariable "q"
        }

    claim' `shouldFailOn` "claim identity_qbit: qbit X Y |- qbit X Y; proof q -> q"
    claim' `shouldFailOn` "claim identity_qbit: qbit X Y |- qbit X Y proof q -> q"
    claim' `shouldFailOn` "claim identity_qbit: qbit X Y |- qbit X Y; proof q ->"
    claim' `shouldFailOn` "claim identity_qbit: qbit X Y |- qbit X Y; proof q"
    claim' `shouldFailOn` "claim identity_qbit: qbit X Y |- qbit X Y; proof"
    claim' `shouldFailOn` "claim identity_qbit: qbit X Y |- qbit X Y;"
    claim' `shouldFailOn` "claim claim: qbit X Y |- qbit X Y;"
    claim' `shouldFailOn` "claim proof: qbit X Y |- qbit X Y;"
