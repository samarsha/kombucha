module Kombucha.Test.InferenceSpec where

import Data.List.NonEmpty
import qualified Data.Map as Map
import qualified Data.Set as Set
import Kombucha.Inference
import Kombucha.SyntaxTree
import Kombucha.TwoOrMore
import Test.Hspec

spec :: Spec
spec = describe "type inference" $ do
  it "infers expression types" $ do
    let exprType' = exprType mempty

    exprType' ExprUnit `shouldBe` Right ([] :=> TypeResource ResourceUnit)

    exprType' (ExprTuple $ TwoOrMore ExprUnit ExprUnit [])
      `shouldBe` Right
        ([] :=> TypeResource (ResourceTuple $ TwoOrMore (TypeResource ResourceUnit) (TypeResource ResourceUnit) []))

    exprType' (ExprLet PatternUnit ExprUnit) `shouldBe` Right ([] :=> TypeResource ResourceUnit)

    exprType' (ExprBlock $ ExprLet (PatternBind "x") ExprUnit :| [ExprVariable "x"])
      `shouldBe` Right ([] :=> TypeResource ResourceUnit)

    exprType'
      ( ExprBlock $
          ExprLet (PatternBind "x") (ExprTuple $ TwoOrMore ExprUnit ExprUnit [])
            :| [ExprVariable "x"]
      )
      `shouldBe` Right
        ([] :=> TypeResource (ResourceTuple $ TwoOrMore (TypeResource ResourceUnit) (TypeResource ResourceUnit) []))

    exprType' (ExprVariable "x") `shouldBe` Left (UnboundVariable "x")

    exprType' (ExprLet PatternUnit $ ExprTuple $ TwoOrMore ExprUnit ExprUnit [])
      `shouldBe` Left
        ( TypeMismatch
            (TypeResource ResourceUnit)
            (TypeResource $ ResourceTuple $ TwoOrMore (TypeResource ResourceUnit) (TypeResource ResourceUnit) [])
        )

    exprType'
      ( ExprLet (PatternTuple $ TwoOrMore PatternUnit PatternUnit []) $
          ExprTuple $ TwoOrMore ExprUnit ExprUnit [ExprUnit]
      )
      `shouldBe` Left (ArityMismatch [] [TypeResource ResourceUnit])

  it "infers expression types with environment" $ do
    let exprType' =
          exprType $
            Map.fromList
              [ ( "foo",
                  ForAll Set.empty $
                    [] :=> TypeInference (TypeResource ResourceUnit :|- TypeResource (ResourceAtom "atom" []))
                ),
                ( "bar",
                  ForAll (Set.singleton "A") $
                    [IsResource $ TypeVariable "A"] :=> TypeInference (TypeResource ResourceUnit :|- TypeVariable "A")
                ),
                ( "baz",
                  ForAll (Set.singleton "B") $
                    [IsResource $ TypeVariable "B"] :=> TypeInference (TypeVariable "B" :|- TypeVariable "B")
                )
              ]

    exprType' (ExprApply "foo" ExprUnit) `shouldBe` Right ([] :=> TypeResource (ResourceAtom "atom" []))
    exprType' (ExprApply "bar" ExprUnit) `shouldBe` Right ([IsResource $ TypeVariable "b"] :=> TypeVariable "b")

    exprType' (ExprApply "baz" ExprUnit)
      `shouldBe` Right ([IsResource $ TypeResource ResourceUnit] :=> TypeResource ResourceUnit)

    exprType' (ExprApply "baz" $ ExprTuple $ TwoOrMore ExprUnit ExprUnit [])
      `shouldBe` Right
        ( [IsResource $ TypeResource (ResourceTuple $ TwoOrMore (TypeResource ResourceUnit) (TypeResource ResourceUnit) [])]
            :=> TypeResource (ResourceTuple $ TwoOrMore (TypeResource ResourceUnit) (TypeResource ResourceUnit) [])
        )

    exprType' (ExprApply "baz" $ ExprApply "bar" $ ExprApply "baz" ExprUnit)
      `shouldBe` Right
        ( [IsResource $ TypeVariable "f", IsResource $ TypeVariable "f", IsResource $ TypeResource ResourceUnit]
            :=> TypeVariable "f"
        )

    exprType' (ExprApply "foo" $ ExprTuple $ TwoOrMore ExprUnit ExprUnit [])
      `shouldBe` Left
        ( TypeMismatch
            (TypeResource ResourceUnit)
            (TypeResource $ ResourceTuple $ TwoOrMore (TypeResource ResourceUnit) (TypeResource ResourceUnit) [])
        )

    exprType' (ExprApply "bar" $ ExprTuple $ TwoOrMore ExprUnit ExprUnit [])
      `shouldBe` Left
        ( TypeMismatch
            (TypeResource ResourceUnit)
            (TypeResource $ ResourceTuple $ TwoOrMore (TypeResource ResourceUnit) (TypeResource ResourceUnit) [])
        )

  it "checks claims" $ do
    let checkClaim' = checkClaim mempty

    checkClaim'
      Claim
        { name = "identity",
          inference = TypeVariable "A" :|- TypeVariable "A",
          proof = PatternBind "x" `Proves` ExprVariable "x"
        }
      `shouldBe` Right ()

    checkClaim'
      Claim
        { name = "clone",
          inference =
            TypeVariable "A"
              :|- TypeResource (ResourceTuple $ TwoOrMore (TypeVariable "A") (TypeVariable "A") []),
          proof = PatternBind "x" `Proves` ExprTuple (TwoOrMore (ExprVariable "x") (ExprVariable "x") [])
        }
      `shouldBe` Left (UnboundVariable "x")

    checkClaim'
      Claim
        { name = "wrong_identity",
          inference = TypeVariable "A" :|- TypeVariable "A",
          proof = PatternBind "x" `Proves` ExprUnit
        }
      `shouldBe` Left (UnusedVariables ["x"])

    checkClaim'
      Claim
        { name = "transmogrify",
          inference = TypeVariable "A" :|- TypeVariable "B",
          proof = PatternBind "x" `Proves` ExprVariable "x"
        }
      `shouldBe` Left (TypeMismatch (TypeVariable "B") (TypeVariable "A"))

    checkClaim'
      Claim
        { name = "destructor",
          inference = TypeVariable "A" :|- TypeResource ResourceUnit,
          proof = PatternBind "x" `Proves` ExprUnit
        }
      `shouldBe` Left (UnusedVariables ["x"])
