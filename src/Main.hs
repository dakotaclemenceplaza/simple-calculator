module Main where

import System.IO (hFlush, stdout)
import Data.Char (isSpace)

main :: IO ()
main = calc

calc = do
  putStr "> "
  hFlush stdout
  input <- getLine
  case parse input of
    Left e -> print e >> calc
    Right expr -> do
      print $ eval expr
      calc
  
data Expr = I Integer
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
  deriving (Show)

eval :: Expr -> Integer
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y
eval (Div x y) = eval x `div` eval y
eval (I x) = x

-- add negative numbers
parse :: String -> Either String Expr
parse input = go (filter isSpace input) Nothing
  where go "" Nothing = Left "Empty input"
        go "" (Just n) = Right n
        go ('+':xs) (Just num) = fmap (Add num) $ go xs Nothing
        go ('-':xs) (Just num) = case go xs Nothing of
                                   Right (Add x y) -> Right (Add (Sub num x) y)
                                   Right (Sub x y) -> Right (Sub (Sub num x) y) -- minus is not commutative
                                   Right expr -> Right (Sub num expr)
                                   left -> left
        go ('*':xs) (Just num) = let (y, rest) = takeNumber xs
                                 in go rest (Just $ Mul num (I $ read y))
        go ('/':xs) (Just num) = let (y, rest) = takeNumber xs
                                 in go rest (Just $ Div num (I $ read y))
        go s Nothing = let (n, rest) = takeNumber s
                       in go rest (Just $ I (read n))
        go _ _ = Left "Error: malformed expression"
        takeNumber s = break (not . digit) s
          where digit c = c `elem` "0123456789"
