module Main (main) where

import MyLibSpec (myLibSpecs)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hspec (testSpecs)

main :: IO ()
main = do
  specs <-
    concat
      <$> mapM
        testSpecs
        [ myLibSpecs
        ]
  defaultMain
    ( testGroup
        "All Tests"
        [ testGroup "Specs" specs
        ]
    )