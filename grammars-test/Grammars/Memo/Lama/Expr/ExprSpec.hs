{-# LANGUAGE OverloadedStrings #-}

module Grammars.Memo.Lama.Expr.ExprSpec where

import CPS.Parser.Memo (_parse)
import CPS.Stream.Stream (ParserState (..), parserState)
import Data.Bifunctor (first)
import Data.Text qualified as T
import GHC.IO (unsafePerformIO)
import Grammars.Memo.Lama.Expr.Expr (Attribute (Void))
import Grammars.Memo.Lama.Expr.ExprGenerator (genExpr)
import Grammars.Memo.Lama.Expr.ExprParser (basicStart)
import Test.Hspec

exprSpec :: Spec
exprSpec = describe "LamaExpr" $ do
  spec_basicStartRandom

spec_basicStartRandom :: Spec
{-# NOINLINE spec_basicStartRandom #-}
spec_basicStartRandom =
  describe "basicStart on random Expr" $ do
    it "parses random Expr with length 1357" $
      do
        let generated = genExpr 1
        let actual = first (\a -> show $ a Void) <$> _parse basicStart (parserState $ T.pack generated)
        actual `shouldBe` [(expectedExpr, ParserState "" (length generated))]
  where
    expectedExpr :: String
    expectedExpr = unsafePerformIO $ readFile "grammars-test/Grammars/Memo/Lama/Expr/expected.txt"
