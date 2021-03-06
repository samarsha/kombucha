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
    let typeExpr' = typeExpr mempty

    typeExpr' ExprUnit `shouldBe` Right (TypeResource ResourceUnit)

    typeExpr' (ExprTuple $ TwoOrMore ExprUnit ExprUnit [])
      `shouldBe` Right (TypeResource $ ResourceTuple $ TwoOrMore ResourceUnit ResourceUnit [])

    typeExpr' (ExprLet PatternUnit ExprUnit) `shouldBe` Right (TypeResource ResourceUnit)

    typeExpr' (ExprBlock $ ExprLet (PatternBind "x") ExprUnit :| [ExprVariable "x"])
      `shouldBe` Right (TypeResource ResourceUnit)

    typeExpr'
      ( ExprBlock $
          ExprLet (PatternBind "x") (ExprTuple $ TwoOrMore ExprUnit ExprUnit [])
            :| [ExprVariable "x"]
      )
      `shouldBe` Right (TypeResource $ ResourceTuple $ TwoOrMore ResourceUnit ResourceUnit [])

    typeExpr' (ExprVariable "x") `shouldBe` Left (UnboundVariable "x")

    typeExpr' (ExprLet PatternUnit $ ExprTuple $ TwoOrMore ExprUnit ExprUnit [])
      `shouldBe` Left
        ( TypeMismatch
            (TypeResource ResourceUnit)
            (TypeResource $ ResourceTuple $ TwoOrMore ResourceUnit ResourceUnit [])
        )

    typeExpr'
      ( ExprLet (PatternTuple $ TwoOrMore PatternUnit PatternUnit []) $
          ExprTuple $ TwoOrMore ExprUnit ExprUnit [ExprUnit]
      )
      `shouldBe` Left (ArityMismatch [] [TypeResource ResourceUnit])

  it "infers expression types with non-empty environment" $ do
    let typeExpr' =
          typeExpr $
            Map.fromList
              [ ("foo", ForAll [] $ TypeInference $ ResourceUnit :|- ResourceAtom "atom" []),
                ("bar", ForAll ["A"] $ TypeInference $ ResourceUnit :|- ResourceVariable "A"),
                ("baz", ForAll ["B"] $ TypeInference $ ResourceVariable "B" :|- ResourceVariable "B")
              ]

    typeExpr' (ExprApply "foo" ExprUnit) `shouldBe` Right (TypeResource $ ResourceAtom "atom" [])
    typeExpr' (ExprApply "bar" ExprUnit) `shouldBe` Right (TypeResource $ ResourceVariable "c")
    typeExpr' (ExprApply "baz" ExprUnit) `shouldBe` Right (TypeResource ResourceUnit)

    typeExpr' (ExprApply "baz" $ ExprTuple $ TwoOrMore ExprUnit ExprUnit [])
      `shouldBe` Right (TypeResource $ ResourceTuple $ TwoOrMore ResourceUnit ResourceUnit [])

    typeExpr' (ExprApply "baz" $ ExprApply "bar" $ ExprApply "baz" ExprUnit)
      `shouldBe` Right (TypeResource $ ResourceVariable "i")

    typeExpr' (ExprApply "foo" $ ExprTuple $ TwoOrMore ExprUnit ExprUnit [])
      `shouldBe` Left
        ( TypeMismatch
            (TypeResource ResourceUnit)
            (TypeResource $ ResourceTuple $ TwoOrMore ResourceUnit ResourceUnit [])
        )

    typeExpr' (ExprApply "bar" $ ExprTuple $ TwoOrMore ExprUnit ExprUnit [])
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
        { name = "fake_identity",
          inference = ForAll ["A"] $ ResourceVariable "A" :|- ResourceVariable "A",
          proof = PatternBind "x" `Proves` ExprUnit
        }
      `shouldBe` Left (TypeMismatch (TypeResource ResourceUnit) (TypeResource ResourceUnit))

    checkClaim'
      Claim
        { name = "transmogrify",
          inference = ForAll ["A", "B"] $ ResourceVariable "A" :|- ResourceVariable "B",
          proof = PatternBind "x" `Proves` ExprVariable "x"
        }
      `shouldBe` Left (TypeMismatch (TypeResource ResourceUnit) (TypeResource ResourceUnit))
