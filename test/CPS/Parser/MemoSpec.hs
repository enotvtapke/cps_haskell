{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}

module CPS.Parser.MemoSpec where

import CPS.Parser.Base (DeterministicAlternative (..))
import CPS.Parser.Memo
  ( Key (..),
    Parser (key),
    makeStableKey,
  )
import CPS.Parser.Primitives
  ( MonadParser (chunk),
  )
import Control.Applicative (Alternative (empty), (<|>))
import Test.Hspec

memoSpec :: Spec
memoSpec = describe "Memo" $ do
  spec_key
  spec_makeStableKey
  spec_bind
  spec_alt
  spec_fmap
  spec_pure
  spec_deterministicAlt

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
  where
    f :: Int -> Int
    f a = a + 1

spec_bind :: Spec
spec_bind =
  describe "bind" $ do
    it "parser has Nothing key after bind" $
      key (chunk "a" >> chunk "a" :: Parser String String) `shouldBe` Nothing

spec_alt :: Spec
spec_alt =
  describe "alt" $ do
    it "parser has Nothing key after alt" $
      key (chunk "a" <|> chunk "a" :: Parser String String) `shouldBe` Nothing
    it "empty parsers have identical keys" $
      key (empty :: Parser String String) `shouldBe` key (empty :: Parser String String)

spec_fmap :: Spec
spec_fmap =
  describe "fmap" $ do
    it "parser has Nothing key after fmap" $
      key ((<> "b") <$> chunk "a" :: Parser String String) `shouldBe` Nothing

spec_pure :: Spec
spec_pure =
  describe "pure" $ do
    it "pure parser has Nothing key " $
      key (pure "a" :: Parser String String) `shouldBe` Nothing

spec_deterministicAlt :: Spec
spec_deterministicAlt =
  describe "</>" $ do
    it "parser has Nothing key after </>" $
      key (chunk "a" </> chunk "a" :: Parser String String) `shouldBe` Nothing
