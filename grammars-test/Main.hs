module Main (main) where

import Grammars.MiscSpec
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hspec (testSpecs)
import Grammars.Expr.ExprSpec (exprSpec)

main :: IO ()
main = do
  specs <-
    concat
      <$> mapM
        testSpecs
        [ miscSpec,
          exprSpec
        ]
  defaultMain
    ( testGroup
        "Grammars Tests"
        [ testGroup "Specs" specs
        ]
    )
