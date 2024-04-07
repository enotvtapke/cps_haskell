module MiscGrammarsSpec where

import Grammars.Misc (acc, accLongest, polynomial)
import Test.Hspec
import TestHelper (parseString)

miscGrammarsSpec :: Spec
miscGrammarsSpec = describe "MiscGrammars" $ do
  spec_acc
  spec_accLongest
  spec_polynomial

spec_acc :: Spec
spec_acc =
  describe "acc" $ do
    it "parses 'acc'" $
      parseString acc "acc" `shouldBe` [("acc", ""), ("ac", "c"), ("a", "cc")]
    it "parses 'a'" $
      parseString acc "a" `shouldBe` [("a", "")]
    it "does not parse 'cc'" $
      parseString acc "cc" `shouldBe` []

spec_accLongest :: Spec
spec_accLongest =
  describe "accLongest" $ do
    it "parses 'acc'" $
      parseString accLongest "acc" `shouldBe` [("acc", "")]
    it "parses 'a'" $
      parseString accLongest "a" `shouldBe` [("a", "")]
    it "does not parse 'cc'" $
      parseString accLongest "cc" `shouldBe` []

spec_polynomial :: Spec
spec_polynomial =
  describe "polynomial" $ do
    it "parses 'd'" $
      parseString polynomial "d" `shouldBe` [("d", "")]
    it "parses 'ada'" $
      parseString polynomial "ada" `shouldBe` [("ada", "")]
    it "parses 'abcdcba'" $
      parseString polynomial "abcdcba" `shouldBe` [("abcdcba", "")]
    it "parses 'abcaaccbabcdcbabccaacba'" $
      parseString polynomial "abcaaccbabcdcbabccaacba" `shouldBe` [("abcaaccbabcdcbabccaacba", "")]
    it "does not parse 'bda'" $
      parseString polynomial "bda" `shouldBe` []
    it "does not parse 'abccaccbabcdcbabccaacba'" $
      parseString polynomial "abccaccbabcdcbabccaacba" `shouldBe` []
