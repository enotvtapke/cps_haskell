module Main (main) where

import Grammars.Base.Expr.ExprSpec (baseExprSpec)
import Grammars.Base.MiscSpec (baseMiscSpec)
import Grammars.Memo.Expr.ExprSpec (exprSpec)
import Grammars.Memo.MiscSpec (miscSpec)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hspec (testSpecs)
import Grammars.Memo.Expr.ExprFastSpec (exprFastSpec)
import Grammars.Memo.Expr.ExprNoLeftRecSpec (exprNoLeftRecSpec)

main :: IO ()
main = do
  baseSpecs <-
    concat
      <$> mapM
        testSpecs
        [ baseMiscSpec,
          baseExprSpec
        ]
  specs <-
    concat
      <$> mapM
        testSpecs
        [ miscSpec,
          exprSpec,
          exprFastSpec,
          exprNoLeftRecSpec
        ]
  defaultMain
    ( testGroup
        "Grammars Tests"
        [ testGroup "Base Specs" baseSpecs,
          testGroup "Specs" specs
        ]
    )
