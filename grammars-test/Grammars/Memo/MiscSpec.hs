{-# LANGUAGE OverloadedStrings #-}

module Grammars.Memo.MiscSpec where

import CPS.Parser.Memo (_parse)
import Grammars.Memo.Misc (acc, accLongest, indirect, polynomial)
import Test.Hspec

miscSpec :: Spec
miscSpec = describe "Misc" $ do
  spec_acc
  spec_accLongest
  spec_polynomial
  spec_indirect

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

spec_indirect :: Spec
spec_indirect =
  describe "indirect" $ do
    it "parses 'a'" $
      _parse indirect "a" `shouldBe` [("a", "")]
    it "parses 'abb'" $
      _parse indirect "abb" `shouldBe` [("abb", ""), ("ab", "b"), ("a", "bb")]
    it "does not parse 'bb'" $
      _parse indirect "bb" `shouldBe` []
