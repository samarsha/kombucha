module Kombucha.Test.ParserSpec where

import Data.List.NonEmpty
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

    resource' "A" `shouldParse` ResourceVariable "A"
    resource' "(A)" `shouldParse` ResourceVariable "A"
    resource' "a" `shouldParse` ResourceVariable "a"
    resource' "(a)" `shouldParse` ResourceVariable "a"

    resource' "qbit" `shouldParse` ResourceAtom "qbit" []
    resource' "qbit X" `shouldParse` ResourceAtom "qbit" [ParamVariable "X"]
    resource' "qbit X Y" `shouldParse` ResourceAtom "qbit" [ParamVariable "X", ParamVariable "Y"]
    resource' "qbit Alice Y" `shouldParse` ResourceAtom "qbit" [ParamValue "Alice", ParamVariable "Y"]
    resource' "qbit Alice Bob" `shouldParse` ResourceAtom "qbit" [ParamValue "Alice", ParamValue "Bob"]

    resource' "A + B" `shouldParse` ResourceTuple (TwoOrMore (ResourceVariable "A") (ResourceVariable "B") [])
    resource' "A + B" `shouldParse` ResourceTuple (TwoOrMore (ResourceVariable "A") (ResourceVariable "B") [])

    resource' "A + B + C"
      `shouldParse` ResourceTuple (TwoOrMore (ResourceVariable "A") (ResourceVariable "B") [ResourceVariable "C"])

    resource' "(A + B) + C"
      `shouldParse` ResourceTuple
        ( TwoOrMore
            (ResourceTuple $ TwoOrMore (ResourceVariable "A") (ResourceVariable "B") [])
            (ResourceVariable "C")
            []
        )

    resource' "(qbit X Y + qbit X Y) + qbit X Y"
      `shouldParse` ResourceTuple
        ( TwoOrMore
            ( ResourceTuple $
                TwoOrMore
                  (ResourceAtom "qbit" [ParamVariable "X", ParamVariable "Y"])
                  (ResourceAtom "qbit" [ParamVariable "X", ParamVariable "Y"])
                  []
            )
            (ResourceAtom "qbit" [ParamVariable "X", ParamVariable "Y"])
            []
        )

    resource' "0" `shouldParse` ResourceUnit
    resource' "0 A" `shouldParse` ResourceUnit

    resource' "5 A"
      `shouldParse` ResourceTuple
        ( TwoOrMore
            (ResourceVariable "A")
            (ResourceVariable "A")
            [ResourceVariable "A", ResourceVariable "A", ResourceVariable "A"]
        )

    resource' "2 qbit X Y"
      `shouldParse` ResourceTuple
        ( TwoOrMore
            (ResourceAtom "qbit" [ParamVariable "X", ParamVariable "Y"])
            (ResourceAtom "qbit" [ParamVariable "X", ParamVariable "Y"])
            []
        )

    resource' "2 A + 3 B"
      `shouldParse` ResourceTuple
        ( TwoOrMore
            (ResourceTuple (TwoOrMore (ResourceVariable "A") (ResourceVariable "A") []))
            (ResourceTuple (TwoOrMore (ResourceVariable "B") (ResourceVariable "B") [ResourceVariable "B"]))
            []
        )

    resource' "2 qbit X Y + 3 qbit X Y"
      `shouldParse` ResourceTuple
        ( TwoOrMore
            ( ResourceTuple
                ( TwoOrMore
                    (ResourceAtom "qbit" [ParamVariable "X", ParamVariable "Y"])
                    (ResourceAtom "qbit" [ParamVariable "X", ParamVariable "Y"])
                    []
                )
            )
            ( ResourceTuple
                ( TwoOrMore
                    (ResourceAtom "qbit" [ParamVariable "X", ParamVariable "Y"])
                    (ResourceAtom "qbit" [ParamVariable "X", ParamVariable "Y"])
                    [ResourceAtom "qbit" [ParamVariable "X", ParamVariable "Y"]]
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
            ForAll ["X", "Y"] $
              ResourceAtom "qbit" [ParamVariable "X", ParamVariable "Y"]
                `Infers` ResourceAtom "ebit" [ParamVariable "X", ParamVariable "Y"]
        }

    axiom' "axiom flip_ebit: ebit X Y |- ebit Y X;"
      `shouldParse` Axiom
        { name = "flip_ebit",
          inference =
            ForAll ["X", "Y"] $
              ResourceAtom "ebit" [ParamVariable "X", ParamVariable "Y"]
                `Infers` ResourceAtom "ebit" [ParamVariable "Y", ParamVariable "X"]
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
            ForAll ["X", "Y"] $
              ResourceAtom "qbit" [ParamVariable "X", ParamVariable "Y"]
                `Infers` ResourceAtom "qbit" [ParamVariable "X", ParamVariable "Y"],
          proof = PatternBind "q" `Proves` ExprVariable "q"
        }

    claim' `shouldFailOn` "claim identity_qbit: qbit X Y |- qbit X Y; proof q -> q"
    claim' `shouldFailOn` "claim identity_qbit: qbit X Y |- qbit X Y proof q -> q"
    claim' `shouldFailOn` "claim identity_qbit: qbit X Y |- qbit X Y; proof q ->"
    claim' `shouldFailOn` "claim identity_qbit: qbit X Y |- qbit X Y; proof q"
    claim' `shouldFailOn` "claim identity_qbit: qbit X Y |- qbit X Y; proof"
    claim' `shouldFailOn` "claim identity_qbit: qbit X Y |- qbit X Y;"
    claim' `shouldFailOn` "claim claim: qbit X Y |- qbit X Y;"
    claim' `shouldFailOn` "claim proof: qbit X Y |- qbit X Y;"

  it "parses patterns" $ do
    let pattern' = parse pattern ""

    pattern' "0" `shouldParse` PatternUnit
    pattern' "a" `shouldParse` PatternBind "a"
    pattern' "(a)" `shouldParse` PatternBind "a"
    pattern' "a + b" `shouldParse` PatternTuple (TwoOrMore (PatternBind "a") (PatternBind "b") [])

    pattern' "(a + b) + c"
      `shouldParse` PatternTuple
        ( TwoOrMore
            (PatternTuple $ TwoOrMore (PatternBind "a") (PatternBind "b") [])
            (PatternBind "c")
            []
        )

    pattern' "(a1 + a2) + (b1 + b2 + b3)"
      `shouldParse` PatternTuple
        ( TwoOrMore
            (PatternTuple $ TwoOrMore (PatternBind "a1") (PatternBind "a2") [])
            (PatternTuple $ TwoOrMore (PatternBind "b1") (PatternBind "b2") [PatternBind "b3"])
            []
        )

  it "parses expressions" $ do
    let expr' = parse expr ""

    expr' "0" `shouldParse` ExprUnit
    expr' "a" `shouldParse` ExprVariable "a"

    expr' "a + (b + c)"
      `shouldParse` ExprTuple
        ( TwoOrMore
            (ExprVariable "a")
            (ExprTuple (TwoOrMore (ExprVariable "b") (ExprVariable "c") []))
            []
        )

    expr' "{ let result = b + a; result }"
      `shouldParse` ExprBlock
        ( ExprLet (PatternBind "result") (ExprTuple $ TwoOrMore (ExprVariable "b") (ExprVariable "a") [])
            :| [ExprVariable "result"]
        )

    expr' "a1 + a2 + b1 + b2 + b3"
      `shouldParse` ExprTuple
        (TwoOrMore (ExprVariable "a1") (ExprVariable "a2") [ExprVariable "b1", ExprVariable "b2", ExprVariable "b3"])

    expr' "{ a }" `shouldParse` ExprBlock (ExprVariable "a" :| [])

    expr' "{ let q2 = identity_qbit q; q2 }"
      `shouldParse` ExprBlock
        ( ExprLet (PatternBind "q2") (ExprApply "identity_qbit" $ ExprVariable "q")
            :| [ExprVariable "q2"]
        )

    expr' "{ let q2 = { let q3 = identity_qbit q; let q4 = identity_qbit q3; identity_qbit q4 }; q2 }"
      `shouldParse` ExprBlock
        ( ExprLet
            (PatternBind "q2")
            ( ExprBlock
                ( ExprLet (PatternBind "q3") (ExprApply "identity_qbit" $ ExprVariable "q")
                    :| [ ExprLet (PatternBind "q4") (ExprApply "identity_qbit" (ExprVariable "q3")),
                         ExprApply "identity_qbit" (ExprVariable "q4")
                       ]
                )
            )
            :| [ExprVariable "q2"]
        )

    expr' `shouldFailOn` "1"
    expr' `shouldFailOn` "{"
    expr' `shouldFailOn` "{ }"
    expr' `shouldFailOn` "{ let result = b + a; result "
    expr' `shouldFailOn` "{ let result = b + a; result; }"
    expr' `shouldFailOn` "{ let q2 = { let q3 = identity_qbit q; identity_qbit q3 } q2 }"

  it "parses documents" $ do
    let document' = parse document ""

    document' "" `shouldParse` []
    document' "resource foo;" `shouldParse` [DeclareResource $ ResourceSpec {name = "foo", parameters = []}]

    document' "parameter Party = Alice | Bob; resource qbit Party Party;"
      `shouldParse` [ DeclareParam $ ParamSpec {name = "Party", values = TwoOrMore "Alice" "Bob" []},
                      DeclareResource $ ResourceSpec {name = "qbit", parameters = ["Party", "Party"]}
                    ]

    document' `shouldFailOn` "resource foo"
