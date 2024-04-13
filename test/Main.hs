module Main (main) where

import CPS.Parser.PrimitivesSpec
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hspec (testSpecs)
import CPS.Parser.MemoSpec (memoSpec)

main :: IO ()
main = do
  specs <-
    concat
      <$> mapM
        testSpecs
        [ primitivesSpec,
          memoSpec
        ]
  defaultMain
    ( testGroup
        "Main Tests"
        [ testGroup "Specs" specs
        ]
    )
