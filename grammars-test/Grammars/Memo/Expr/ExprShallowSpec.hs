{-# LANGUAGE OverloadedStrings #-}

module Grammars.Memo.Expr.ExprShallowSpec where

import CPS.Parser.Memo (_parse)
import CPS.Stream.Stream (ParserState (..), parserState)
import Data.Text qualified as T
import Grammars.Memo.Expr.ExprShallowParser (stmtsStart)
import Test.Hspec

exprShallowSpec :: Spec
exprShallowSpec = describe "ExprShallow" $ do
  spec_exprStart

spec_exprStart :: Spec
spec_exprStart =
  describe "exprStart" $ do
    it "parses Expr 'y := 42; y + 3; z := y * 2 - 5; z / (4 - 2)'" $
      do
        let generated = "y := 42; y + 3; z := y * 2 - 5; z / (4 - 2)"
        let actual = _parse stmtsStart (parserState $ T.pack generated)
        actual `shouldBe` [(Right 39, ParserState "" (length generated))]
