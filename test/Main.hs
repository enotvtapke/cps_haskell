module Main (main) where

import MiscGrammarsSpec (miscGrammarsSpec)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hspec (testSpecs)

main :: IO ()
main = do
  specs <-
    concat
      <$> mapM
        testSpecs
        [ miscGrammarsSpec
        ]
  defaultMain
    ( testGroup
        "All Tests"
        [ testGroup "Specs" specs
        ]
    )
