{-# LANGUAGE OverloadedStrings #-}

module Grammars.Base.Expr.ExprSpec where

import CPS.Parser.Base (baseParse)
import Data.Bifunctor (first)
import Data.Text qualified as T
import Grammars.Base.Expr.ExprParser (exprStart)
import Grammars.Memo.Expr.Expr
import Grammars.Memo.Expr.ExprGenerator (genExpr)
import Test.Hspec

baseExprSpec :: Spec
baseExprSpec = describe "Expr" $ do
  spec_exprStart
  spec_exprStartRandom

spec_exprStart :: Spec
spec_exprStart =
  describe "exprStart" $ do
    it "parses '42'" $
      baseParse exprStart "42" `shouldBe` [(ExprVal (TermVal (FVal 42)), "")]
    it "parses '99+1'" $
      baseParse exprStart "99+1" `shouldBe` [(ExprOp (ExprVal (TermVal (FVal 99))) "+" (TermVal (FVal 1)), "")]
    it "parses '75/5'" $
      baseParse exprStart "75/5" `shouldBe` [(ExprVal (TermOp (TermVal (FVal 75)) "/" (FVal 5)), "")]
    it "parses '3+1+2'" $
      baseParse exprStart "3+1+2" `shouldBe` [(ExprOp (ExprOp (ExprVal (TermVal (FVal 3))) "+" (TermVal (FVal 1))) "+" (TermVal (FVal 2)), "")]
    it "parses '4*(2-44)'" $
      baseParse exprStart "4*(2-44)" `shouldBe` [(ExprVal (TermOp (TermVal (FVal 4)) "*" (FExpr (ExprOp (ExprVal (TermVal (FVal 2))) "-" (TermVal (FVal 44))))), "")]
    it "does not parse '+12'" $
      baseParse exprStart "+12" `shouldBe` []
    it "does not parse '2^10'" $
      baseParse exprStart "2^10" `shouldBe` []

spec_exprStartRandom :: Spec
spec_exprStartRandom =
  describe "exprStart on random Expr" $ do
    it "parses random Expr with length 100" $
      do
        let generated = show $ genExpr 100
        let actual = first show <$> baseParse exprStart (T.pack generated)
        actual `shouldBe` [(generated, "")]
