module Main where

import Test.Hspec
import Calc (parse, eval)

import Control.Monad.State (runStateT, evalStateT)

main :: IO ()
main = hspec $ do
  describe "tests without variables" $ do
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
      test "7 + 30" `shouldBe` Right 37
      test "25 -3" `shouldBe` Right 22

      test "1/5 + 1/5" `shouldBe` Right 0.4
      test "2+3-8+2" `shouldBe` Right (-1)
      test "22 - 3 - 7" `shouldBe` Right 12
      test "7 + 3*3-2" `shouldBe` Right 14
      test "25 -3 +3 -4 - - 1" `shouldBe` Right 22
      test "2-3 * 6 + 20" `shouldBe` Right 4
      test "200 - 31 - 10" `shouldBe` Right 159
      test "7 * 3 + 1 / 2" `shouldBe` Right 21.5
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

  describe "tests with variables" $ do
    it "works" $ do

      testVar "x" [("x", 3)] `shouldSatisfy` result (Right 3)
      testVar "x + y" [("x", 3),("y", 5)] `shouldSatisfy` result (Right 8)
      testVar "x = 5" [] `shouldSatisfy` state (Right [("x", 5)])
      testVar "x = y + 3" [("y", 2)] `shouldSatisfy` state (Right [("x", 5), ("y", 2)])
      testVar "x = x + 1" [("x", 2)] `shouldSatisfy` state (Right [("x", 3)])
      
    it "shouldn't work" $ do
      
      testVar "x" [] `shouldBe` Left "Error: did not found variable x"
      testVar "3 + y" [] `shouldBe` Left "Error: did not found variable y"
      testVar "x = 3 + y" [("x", 1), ("z", 2)] `shouldBe` Left "Error: did not found variable y"
            
-- test without variables
test input = case parse input of
  Right i -> evalStateT (eval i) []
  Left e -> Left e   -- strange thing, simply "left -> left" doesn't work
  
-- test with variables
testVar input state = case parse input of
  Right i -> runStateT (eval i) state
  Left e -> Left e

-- helper to check the result
result expected res = fmap fst res == expected

-- helper to check the state
state expected res = fmap snd res == expected
