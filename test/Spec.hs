module Main where

import Test.Hspec
import Calc (parse, eval)

main :: IO ()
main = hspec $ do
  describe "general tests" $ do
    it "works" $ do
      
      test "23" `shouldBe` Right 23
      test "0" `shouldBe` Right 0
      test "-3" `shouldBe` Right (-3)

      test "2+3" `shouldBe` Right 5
      test "2+ 31" `shouldBe` Right 33
      test "7 + 3" `shouldBe` Right 10
      test "25 -3" `shouldBe` Right 22

      test "2-3" `shouldBe` Right (-1)
      test "2+ -5" `shouldBe` Right (-3)
      test "7 * 3" `shouldBe` Right 21
      test "26/2" `shouldBe` Right 13

      test "2+3" `shouldBe` Right 5
      test "2+ 31" `shouldBe` Right 33
      test "7 + 3" `shouldBe` Right 10
      test "25 -3" `shouldBe` Right 22

      test "2+3-8+2" `shouldBe` Right (-1)
      test "22 - 3 - 7" `shouldBe` Right 12
      test "7 + 3*3-2" `shouldBe` Right 14
      test "25 -3 +3 -4 - - 1" `shouldBe` Right 22
      test "2-3 * 6 + 20" `shouldBe` Right 4
      test "200 - 31 - 10" `shouldBe` Right 159
      test "7 * 3 + 1 / 2" `shouldBe` Right 21
      test "25*3 - 20 - 6 / 2 - 15 * 2" `shouldBe` Right 22

    it "shouldn't work" $ do
      
      test "" `shouldBe` Left "Empty input"
      test "  " `shouldBe` Left "Empty input"
      test "7 + +3*3-2" `shouldBe` Left "Error: malformed expression" 
      test "25 -3 / +3 -4 - - 1" `shouldBe` Left "Error: malformed expression" 
      test "2-3 * 6 + --20" `shouldBe` Left "Error: malformed expression" 
      test "200 - (31 - 10" `shouldBe` Left "Error: malformed expression"
      test "200 - ((31 - )10)" `shouldBe` Left "Error: malformed expression" 
      test "7 *** 3 + 1 / 2" `shouldBe` Left "Error: malformed expression" 
      test "25*3 - 20 - 6 / 2 +-+ 15 * 2" `shouldBe` Left "Error: malformed expression" 

-- general test
test = fmap eval . parse
