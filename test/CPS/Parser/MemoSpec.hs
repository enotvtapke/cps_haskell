{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}

module CPS.Parser.MemoSpec where

import CPS.Parser.Memo (Key (..), Parser, makeStableKey, makeUniqueKey)
import CPS.Parser.Primitives (MonadParser (chunk))
import Control.Monad (replicateM_)
import Test.Hspec
import CPS.Parser.Memo (Parser(key))
import Control.Applicative ((<|>))

memoSpec :: Spec
memoSpec = describe "Memo" $ do
  spec_key
  spec_makeStableKey
  spec_makeRandomStableKey
  spec_bind
  spec_alt
  spec_fmap
  spec_pure

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

spec_makeRandomStableKey :: Spec
spec_makeRandomStableKey =
  describe "makeUniqueKey" $ do
    it "makeUniqueKey is not equal to self" $
      replicateM_ 100 (makeUniqueKey () `shouldNotBe` makeUniqueKey ())

spec_bind :: Spec
spec_bind =
  describe "bind" $ do
    it "parsers have different keys after bind" $
      key (chunk "a" >> chunk "a" :: Parser String String) `shouldNotBe` key (chunk "a" >> chunk "a" :: Parser String String)

spec_alt :: Spec
spec_alt =
  describe "alt" $ do
    it "parsers have different keys after alt" $
      key (chunk "a" <|> chunk "a" :: Parser String String) `shouldNotBe` key (chunk "a" <|> chunk "a" :: Parser String String)

spec_fmap :: Spec
spec_fmap =
  describe "fmap" $ do
    it "parsers have different keys after fmap" $
      key ((<> "b") <$> chunk "a" :: Parser String String) `shouldNotBe` key ((<> "b") <$> chunk "a" :: Parser String String)

spec_pure :: Spec
spec_pure =
  describe "pure" $ do
    it "pure parsers have different keys " $
      key (pure "a" :: Parser String String) `shouldNotBe` key (pure "a" :: Parser String String)
