{-# LANGUAGE OverloadedStrings #-}

module Grammars.Memo.Expr.ExprFastSpec where

import CPS.Parser.Memo (_parse)
import CPS.Stream.Stream (ParserState (..), parserState)
import Data.Bifunctor (first)
import Data.Text qualified as T
import Grammars.Memo.Expr.ExprFastParser (exprStart)
import Grammars.Memo.Expr.ExprGenerator (genExpr)
import Test.Hspec

exprFastSpec :: Spec
exprFastSpec = describe "ExprFast" $ do
  spec_exprStartRandom

spec_exprStartRandom :: Spec
spec_exprStartRandom =
  describe "exprStart on random Expr" $ do
    it "parses random Expr with length 1000" $
      do
        let generated = show $ genExpr 1000
        let actual = first show <$> _parse exprStart (parserState $ T.pack generated)
        actual `shouldBe` [(generated, ParserState "" (length generated))]
