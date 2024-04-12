module CPS.Parser.PrimitivesSpec where

import CPS.Parser.Base (baseParse)
import CPS.Parser.Primitives (regex)
import Test.Hspec

primitivesSpec :: Spec
primitivesSpec = describe "Primitives" $ do
  spec_regex

spec_regex :: Spec
spec_regex =
  describe "regex" $ do
    it "r'[0-9]*\\.[0-9]+' parses '9162.420abc'" $
      baseParse (regex "[0-9]*\\.[0-9]+") "9162.420" `shouldBe` [("9162.420", "")]
    it "r'[0-9]' does not parse 'a12'" $
      baseParse (regex "[0-9]") "a12" `shouldBe` []
    it "r'[0-9]*' partially parses '7812abc'" $
      baseParse (regex "[0-9]*") "7812abc" `shouldBe` [("7812", "abc")]
