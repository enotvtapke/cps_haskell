{-# OPTIONS_GHC -fno-cse #-}

module CPS.Parser.MemoSpec where

import CPS.Parser.Memo (Key (..), makeStableKey, makeUniqueKey)
import Control.Monad (replicateM_)
import Test.Hspec

memoSpec :: Spec
memoSpec = describe "Memo" $ do
  spec_key
  spec_makeStableKey
  spec_makeRandomStableKey

spec_key :: Spec
spec_key =
  describe "key" $ do
    it "simple keys are equal" $ do
      Key "a" `shouldBe` Key "a"
      Key () `shouldBe` Key ()
    it "complex keys are equal" $
      Key ("a", "b") `shouldBe` Key ("a", "b")
    it "simple keys are not equal" $
      Key "a" `shouldNotBe` Key "b"
    it "complex keys are not equal" $
      Key ("a", "b") `shouldNotBe` Key ("a", "a")
    it "simple keys of different types are not equal" $
      Key (1 :: Int) `shouldNotBe` Key (2 :: Integer)
    it "complex keys of different types are not equal" $
      Key ((), "a", 2 :: Int) `shouldNotBe` Key ((), "a", 2 :: Integer)

spec_makeStableKey :: Spec
spec_makeStableKey =
  describe "makeStableKey" $ do
    it "makeStableKey f is equal to self" $
      makeStableKey f `shouldBe` makeStableKey f
    it "makeStableKey () is equal to self" $
      makeStableKey () `shouldBe` makeStableKey ()
    it "makeStableKey '' is equal to self" $
      makeStableKey "" `shouldBe` makeStableKey ""
    it "makeStableKey (f, f) is not equal to makeStableKey (f, f)" $
      makeStableKey (f, f) `shouldNotBe` makeStableKey (f, f)
  where
    f :: Int -> Int
    f a = a + 1

spec_makeRandomStableKey :: Spec
spec_makeRandomStableKey =
  describe "makeUniqueKey" $ do
    it "makeUniqueKey is not equal to self" $
      replicateM_ 100 (makeUniqueKey () `shouldNotBe` makeUniqueKey ())
