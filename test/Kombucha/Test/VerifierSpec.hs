module Kombucha.Test.VerifierSpec where

import Kombucha.Inference
import Kombucha.SyntaxTree
import Kombucha.TwoOrMore
import Kombucha.Verifier
import Test.Hspec

spec :: Spec
spec = describe "verifier" $ do
  it "verifies claims" $ do
    verify "claim identity: A |- A; proof x -> x;" `shouldBe` Right ()
    verify "claim clone: A |- A + A; proof x -> x + x;" `shouldBe` Left (TypeError $ UnboundVariable "x")
    verify "claim wrong_identity: A |- A; proof x -> 0;" `shouldBe` Left (TypeError $ UnusedVariables ["x"])

    verify "claim transmogrify: A |- B; proof x -> x;"
      `shouldBe` Left
        ( TypeError $
            TypeMismatch
              (TypeResource $ ResourceVariable "B")
              (TypeResource $ ResourceVariable "A")
        )

    verify "claim destructor: A |- 0; proof x -> 0;" `shouldBe` Left (TypeError $ UnusedVariables ["x"])

    verify
      "claim identity: A |- A;\
      \proof x -> x;\
      \\
      \claim identity2: A |- A;\
      \proof x -> identity x;"
      `shouldBe` Right ()

    verify
      "claim identity2: A |- A;\
      \proof x -> identity x;\
      \\
      \claim identity: A |- A;\
      \proof x -> x;"
      `shouldBe` Left (TypeError $ UnboundVariable "identity")

    verify "claim recursive: A |- B; proof x -> recursive x;"
      `shouldBe` Left (TypeError $ UnboundVariable "recursive")

    verify
      "claim associative: (A + B) + C |- A + (B + C);\
      \proof (x + y) + z -> x + (y + z);"
      `shouldBe` Right ()

    verify
      "claim commutative: A + B |- B + A;\
      \proof x + y -> y + x;"
      `shouldBe` Right ()

    verify
      "claim shadow: 0 |- 0 + 0;\
      \proof 0 -> {\
      \    let x = 0;\
      \    let y = {\
      \        let x = 0;\
      \        x\
      \    };\
      \    x + y\
      \};"
      `shouldBe` Left (TypeError $ AlreadyBound "x")

    verify
      "claim shadow: 0 |- 0;\
      \proof x -> {\
      \    let x = 0;\
      \    x\
      \};"
      `shouldBe` Left (TypeError $ AlreadyBound "x")

  it "verifies claims with axioms" $ do
    verify
      "axiom create: 0 |- A;\
      \\
      \claim my_create: 0 |- A;\
      \proof 0 -> create 0;"
      `shouldBe` Right ()

    verify
      "axiom destroy: A |- 0;\
      \\
      \claim my_destroy: A |- 0;\
      \proof x -> destroy x;"
      `shouldBe` Right ()

    verify
      "axiom create: 0 |- A;\
      \axiom destroy: A |- 0;\
      \\
      \claim create_and_destroy: 0 |- 0;\
      \proof 0 -> destroy (create 0);"
      `shouldBe` Right ()

    verify
      "axiom nothing: 0 |- 0;\
      \\
      \claim create: 0 |- A;\
      \proof 0 -> nothing 0;"
      `shouldBe` Left
        ( TypeError $
            TypeMismatch
              (TypeResource $ ResourceVariable "A")
              (TypeResource ResourceUnit)
        )

    verify
      "axiom clone: A |- A + A;\
      \\
      \claim double_clone: A |- (A + A) + (A + A);\
      \proof x -> clone (clone x);"
      `shouldBe` Right ()

    verify
      "axiom clone: A |- A + A;\
      \\
      \claim double_clone: A |- (A + A) + (A + A);\
      \proof x -> clone x;"
      `shouldBe` Left
        ( TypeError $
            TypeMismatch
              (TypeResource $ ResourceTuple $ TwoOrMore (ResourceVariable "A") (ResourceVariable "A") [])
              (TypeResource $ ResourceVariable "A")
        )

    verify
      "axiom clone: A |- A + A;\
      \\
      \claim quadruple: A |- A + A + A + A;\
      \proof x -> {\
      \    let (a + b) + (c + d) = clone (clone x);\
      \    a + b + c + d\
      \};"
      `shouldBe` Right ()
