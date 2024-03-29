module MyLibSpec where
import Test.Hspec
import MyLib (someOtherFunc)

myLibSpecs :: Spec
myLibSpecs = describe "MyLib" spec_someOtherFunc

spec_someOtherFunc :: Spec
spec_someOtherFunc =
  describe "someOtherFunc" $ do
    it "someOtherFunc returns 'someOtherFunc'" $
      someOtherFunc `shouldBe` "someOtherFunc"
    it "someOtherFunc does not return 'someFunc'" $
      someOtherFunc `shouldNotBe` "someFunc"
