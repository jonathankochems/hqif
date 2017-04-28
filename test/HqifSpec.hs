module HqifSpec (main, spec) where

import Test.Hspec
import Lib
import LibTwo

main :: IO ()
main = hspec spec

spec :: Spec
spec = 
  describe "someFunction" $ 
    it "should work fine" $ do
      x <- someFunc
      x `shouldBe` ()
      (y,_y) <- someOtherFunc 42
      _y `shouldBe` (show y) 
      True `shouldBe` True
      x <- someFuncThree
      x `shouldBe` ()
      (y,_y) <- someOtherFuncFour 41
      _y `shouldBe` (show y) 
