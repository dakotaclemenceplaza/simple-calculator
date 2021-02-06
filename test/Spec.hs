module Main where

import Test.Hspec
import Calc (parse, eval)

main :: IO ()
main = hspec $ do
  describe "test the tests" $
    it "works" $ do
    test "23" `shouldBe` Right 23

-- general test
test = fmap eval . parse
