module Main (main) where

import CPS.Parser.PrimitivesSpec
import MiscGrammarsSpec
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hspec (testSpecs)

main :: IO ()
main = do
  specs <-
    concat
      <$> mapM
        testSpecs
        [ miscGrammarsSpec,
          primitivesSpec
        ]
  defaultMain
    ( testGroup
        "All Tests"
        [ testGroup "Specs" specs
        ]
    )
