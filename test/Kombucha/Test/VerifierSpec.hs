module Kombucha.Test.VerifierSpec where

import Kombucha.Inference
import Kombucha.SyntaxTree
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
