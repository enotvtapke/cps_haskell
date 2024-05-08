{-# LANGUAGE OverloadedStrings #-}

module Grammars.Memo.Expr.ExprNoLeftRecSpec where

import CPS.Parser.Base (baseParse)
import CPS.Stream.Stream (ParserState (..), parserState)
import Data.Bifunctor (first)
import Data.Text qualified as T
import Grammars.Memo.Expr.ExprGenerator (genExpr)
import Grammars.Memo.Expr.ExprNoLeftRecParser (exprStart)
import Test.Hspec

exprNoLeftRecSpec :: Spec
exprNoLeftRecSpec = describe "ExprNoLeftRec" $ do
  spec_exprStartRandom

spec_exprStartRandom :: Spec
spec_exprStartRandom =
  describe "exprStart on random Expr" $ do
    it "parses random Expr with length 1000" $
      do
        let generated = show $ genExpr 1000
        let actual = first show <$> baseParse exprStart (parserState $ T.pack generated)
        actual `shouldBe` [(generated, ParserState "" (length generated))]
