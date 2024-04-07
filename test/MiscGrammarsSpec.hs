{-# LANGUAGE OverloadedStrings #-}

module MiscGrammarsSpec where

import CPS.Parser.Core (_parse)
import Grammars.Misc (acc, accLongest, polynomial)
import Test.Hspec

miscGrammarsSpec :: Spec
miscGrammarsSpec = describe "MiscGrammars" $ do
  spec_acc
  spec_accLongest
  spec_polynomial

spec_acc :: Spec
spec_acc =
  describe "acc" $ do
    it "parses 'acc'" $
      _parse acc "acc" `shouldBe` [("acc", ""), ("ac", "c"), ("a", "cc")]
    it "parses 'a'" $
      _parse acc "a" `shouldBe` [("a", "")]
    it "does not parse 'cc'" $
      _parse acc "cc" `shouldBe` []

spec_accLongest :: Spec
spec_accLongest =
  describe "accLongest" $ do
    it "parses 'acc'" $
      _parse accLongest "acc" `shouldBe` [("acc", "")]
    it "parses 'a'" $
      _parse accLongest "a" `shouldBe` [("a", "")]
    it "does not parse 'cc'" $
      _parse accLongest "cc" `shouldBe` []

spec_polynomial :: Spec
spec_polynomial =
  describe "polynomial" $ do
    it "parses 'd'" $
      _parse polynomial "d" `shouldBe` [("d", "")]
    it "parses 'ada'" $
      _parse polynomial "ada" `shouldBe` [("ada", "")]
    it "parses 'abcdcba'" $
      _parse polynomial "abcdcba" `shouldBe` [("abcdcba", "")]
    it "parses 'abcaaccbabcdcbabccaacba'" $
      _parse polynomial "abcaaccbabcdcbabccaacba" `shouldBe` [("abcaaccbabcdcbabccaacba", "")]
    it "does not parse 'bda'" $
      _parse polynomial "bda" `shouldBe` []
    it "does not parse 'abccaccbabcdcbabccaacba'" $
      _parse polynomial "abccaccbabcdcbabccaacba" `shouldBe` []
