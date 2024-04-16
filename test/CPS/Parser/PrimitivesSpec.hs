{-# OPTIONS_GHC -fno-cse -fno-cmm-elim-common-blocks #-}

module CPS.Parser.PrimitivesSpec where

import CPS.Parser.Base (baseParse)
import CPS.Parser.Memo (Parser (key), _parse)
import CPS.Parser.Primitives (MonadParser (chunk, eof), regex, single)
import Test.Hspec

primitivesSpec :: Spec
primitivesSpec = describe "Primitives" $ do
  spec_regex
  spec_chunk
  spec_eof
  spec_single

spec_regex :: Spec
spec_regex =
  describe "regex" $ do
    it "r'[0-9]*\\.[0-9]+' parses '9162.420abc'" $
      baseParse (regex "[0-9]*\\.[0-9]+") "9162.420" `shouldBe` [("9162.420", "")]
    it "r'[0-9]' does not parse 'a12'" $
      baseParse (regex "[0-9]") "a12" `shouldBe` []
    it "r'[0-9]*' partially parses '7812abc'" $
      baseParse (regex "[0-9]*") "7812abc" `shouldBe` [("7812", "abc")]

spec_chunk :: Spec
spec_chunk =
  describe "chunk" $ do
    it "parsers for the same chunks have identical keys" $
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
    it "parsers for single have different keys" $
      key (single 'a' :: Parser String Char) `shouldNotBe` key (single 'b' :: Parser String Char)
    it "parsers for single have different keys even if arguments are the same" $
      key (single 'a' :: Parser String Char) `shouldNotBe` key (single 'a' :: Parser String Char)
    it "single 'a' parses 'a'" $
      _parse (single 'a' :: Parser String Char) "a" `shouldBe` [('a', "")]
    it "single 'a' does not parse 'b'" $
      _parse (single 'a' :: Parser String Char) "b" `shouldBe` []
