{-# OPTIONS_GHC -fno-cse -fno-cmm-elim-common-blocks #-}

module CPS.Parser.PrimitivesSpec where

import CPS.Parser.Base (DeterministicAlternative (..), baseParse)
import CPS.Parser.Memo (Parser (key), _parse)
import CPS.Parser.Primitives
  ( MonadParser (..),
    regex,
    single,
  )
import Test.Hspec

primitivesSpec :: Spec
primitivesSpec = describe "Primitives" $ do
  spec_regex
  spec_chunk
  spec_eof
  spec_single
  spec_deterministicAlt
  spec_count

spec_regex :: Spec
spec_regex =
  describe "regex" $ do
    it "parsers for same regexes have identical keys" $
      key (regex "[0-9]" :: Parser String String) `shouldBe` key (regex "[0-9]" :: Parser String String)
    it "parsers for different regexes have different keys" $
      key (regex "[0-9]a" :: Parser String String) `shouldNotBe` key (regex "[0-9]b" :: Parser String String)
    it "r'[0-9]*\\.[0-9]+' parses '9162.420abc'" $
      baseParse (regex "[0-9]*\\.[0-9]+") "9162.420" `shouldBe` [("9162.420", "")]
    it "r'[0-9]' does not parse 'a12'" $
      baseParse (regex "[0-9]") "a12" `shouldBe` []
    it "r'[0-9]*' partially parses '7812abc'" $
      baseParse (regex "[0-9]*") "7812abc" `shouldBe` [("7812", "abc")]

spec_chunk :: Spec
spec_chunk =
  describe "chunk" $ do
    it "parsers for same chunks have identical keys" $
      key (chunk "one" :: Parser String String) `shouldBe` key (chunk "one" :: Parser String String)
    it "parsers for different chunks have different keys" $
      key (chunk "rice" :: Parser String String) `shouldNotBe` key (chunk "rome" :: Parser String String)

spec_eof :: Spec
spec_eof =
  describe "eof" $ do
    it "parsers for eof have identical keys" $
      key (eof :: Parser String ()) `shouldBe` key (eof :: Parser String ())
    it "parses ''" $
      _parse (eof :: Parser String ()) "" `shouldBe` [((), "")]
    it "does not parse 'not_empty_string'" $
      _parse (eof :: Parser String ()) "not_empty_string" `shouldBe` []

spec_single :: Spec
spec_single =
  describe "single" $ do
    it "parsers for different tokens have different keys" $
      key (single 'a' :: Parser String Char) `shouldNotBe` key (single 'b' :: Parser String Char)
    it "parsers for same tokens have identical keys" $
      key (single 'a' :: Parser String Char) `shouldBe` key (single 'a' :: Parser String Char)
    it "single 'a' parses 'a'" $
      _parse (single 'a' :: Parser String Char) "a" `shouldBe` [('a', "")]
    it "single 'a' does not parse 'b'" $
      _parse (single 'a' :: Parser String Char) "b" `shouldBe` []

spec_deterministicAlt :: Spec
spec_deterministicAlt =
  describe "</>" $ do
    it "(</>) is not commutative" $ do
      _parse ((chunk "a" :: Parser String String) </> (chunk "ac" :: Parser String String)) "ac" `shouldBe` [("a", "c")]
      _parse ((chunk "ac" :: Parser String String) </> (chunk "a" :: Parser String String)) "ac" `shouldBe` [("ac", "")]

spec_count :: Spec
spec_count =
  describe "count" $ do
    it "parses aaa" $
      _parse (count (single 'a') 3) "aaa" `shouldBe` [("aaa", "")]
    it "parses aaaa" $
      _parse (count (single 'a') 3) "aaaa" `shouldBe` [("aaa", "a")]
    it "does not parse aa" $
      _parse (count (single 'a') 3) "aa" `shouldBe` []
