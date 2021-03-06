module Kombucha.Test.InferenceSpec where

import Data.List.NonEmpty
import qualified Data.Map as Map
import Kombucha.Inference
import Kombucha.SyntaxTree
import Kombucha.TwoOrMore
import Test.Hspec

spec :: Spec
spec = describe "type inference" $ do
  it "infers expression types with empty environment" $ do
    let exprType' = exprType mempty

    exprType' ExprUnit `shouldBe` Right (TypeResource ResourceUnit)

    exprType' (ExprTuple $ TwoOrMore ExprUnit ExprUnit [])
      `shouldBe` Right (TypeResource $ ResourceTuple $ TwoOrMore ResourceUnit ResourceUnit [])

    exprType' (ExprLet PatternUnit ExprUnit) `shouldBe` Right (TypeResource ResourceUnit)

    exprType' (ExprBlock $ ExprLet (PatternBind "x") ExprUnit :| [ExprVariable "x"])
      `shouldBe` Right (TypeResource ResourceUnit)

    exprType'
      ( ExprBlock $
          ExprLet (PatternBind "x") (ExprTuple $ TwoOrMore ExprUnit ExprUnit [])
            :| [ExprVariable "x"]
      )
      `shouldBe` Right (TypeResource $ ResourceTuple $ TwoOrMore ResourceUnit ResourceUnit [])

    exprType' (ExprVariable "x") `shouldBe` Left (UnboundVariable "x")

    exprType' (ExprLet PatternUnit $ ExprTuple $ TwoOrMore ExprUnit ExprUnit [])
      `shouldBe` Left
        ( TypeMismatch
            (TypeResource ResourceUnit)
            (TypeResource $ ResourceTuple $ TwoOrMore ResourceUnit ResourceUnit [])
        )

    exprType'
      ( ExprLet (PatternTuple $ TwoOrMore PatternUnit PatternUnit []) $
          ExprTuple $ TwoOrMore ExprUnit ExprUnit [ExprUnit]
      )
      `shouldBe` Left (ArityMismatch [] [TypeResource ResourceUnit])

  it "infers expression types with non-empty environment" $ do
    let exprType' =
          exprType $
            Map.fromList
              [ ("foo", ForAll [] $ TypeInference $ ResourceUnit :|- ResourceAtom "atom" []),
                ("bar", ForAll ["A"] $ TypeInference $ ResourceUnit :|- ResourceVariable "A"),
                ("baz", ForAll ["B"] $ TypeInference $ ResourceVariable "B" :|- ResourceVariable "B")
              ]

    exprType' (ExprApply "foo" ExprUnit) `shouldBe` Right (TypeResource $ ResourceAtom "atom" [])
    exprType' (ExprApply "bar" ExprUnit) `shouldBe` Right (TypeResource $ ResourceVariable "c")
    exprType' (ExprApply "baz" ExprUnit) `shouldBe` Right (TypeResource ResourceUnit)

    exprType' (ExprApply "baz" $ ExprTuple $ TwoOrMore ExprUnit ExprUnit [])
      `shouldBe` Right (TypeResource $ ResourceTuple $ TwoOrMore ResourceUnit ResourceUnit [])

    exprType' (ExprApply "baz" $ ExprApply "bar" $ ExprApply "baz" ExprUnit)
      `shouldBe` Right (TypeResource $ ResourceVariable "i")

    exprType' (ExprApply "foo" $ ExprTuple $ TwoOrMore ExprUnit ExprUnit [])
      `shouldBe` Left
        ( TypeMismatch
            (TypeResource ResourceUnit)
            (TypeResource $ ResourceTuple $ TwoOrMore ResourceUnit ResourceUnit [])
        )

    exprType' (ExprApply "bar" $ ExprTuple $ TwoOrMore ExprUnit ExprUnit [])
      `shouldBe` Left
        ( TypeMismatch
            (TypeResource ResourceUnit)
            (TypeResource $ ResourceTuple $ TwoOrMore ResourceUnit ResourceUnit [])
        )

  it "checks claims" $ do
    let checkClaim' = checkClaim mempty

    checkClaim'
      Claim
        { name = "identity",
          inference = ForAll ["A"] $ ResourceVariable "A" :|- ResourceVariable "A",
          proof = PatternBind "x" `Proves` ExprVariable "x"
        }
      `shouldBe` Right ()

    checkClaim'
      Claim
        { name = "clone",
          inference =
            ForAll ["A"] $
              ResourceVariable "A"
                :|- ResourceTuple (TwoOrMore (ResourceVariable "A") (ResourceVariable "A") []),
          proof = PatternBind "x" `Proves` ExprTuple (TwoOrMore (ExprVariable "x") (ExprVariable "x") [])
        }
      `shouldBe` Left (UnboundVariable "x")

    checkClaim'
      Claim
        { name = "wrong_identity",
          inference = ForAll ["A"] $ ResourceVariable "A" :|- ResourceVariable "A",
          proof = PatternBind "x" `Proves` ExprUnit
        }
      `shouldBe` Left (UnusedVariables ["x"])

    checkClaim'
      Claim
        { name = "transmogrify",
          inference = ForAll ["A", "B"] $ ResourceVariable "A" :|- ResourceVariable "B",
          proof = PatternBind "x" `Proves` ExprVariable "x"
        }
      `shouldBe` Left (TypeMismatch (TypeResource $ ResourceVariable "B") (TypeResource $ ResourceVariable "A"))

    checkClaim'
      Claim
        { name = "destructor",
          inference = ForAll ["A"] $ ResourceVariable "A" :|- ResourceUnit,
          proof = PatternBind "x" `Proves` ExprUnit
        }
      `shouldBe` Left (UnusedVariables ["x"])