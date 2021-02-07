module Calc where

import System.IO (hFlush, stdout)
import Data.Char (isSpace)
-- import Text.Read (readMaybe)
import Data.Tuple (swap)

calc = do
  putStr "> "
  hFlush stdout
  input <- getLine
  case parse input of
    Left e -> print e >> calc
    Right expr -> do
      putStrLn . cleanDouble . eval $ expr
      calc
  
data Expr = I Double
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
  deriving (Show)

eval :: Expr -> Double
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y
eval (Div x y) = eval x / eval y
eval (I x) = x

cleanDouble = reverse . dropWhile (`elem` "0.") . reverse . show
  
-- Not the best handling of errors
parse :: String -> Either String Expr
parse input | filter (not . isSpace) input == "" = Left "Empty input"
parse input = case go (filter (not . isSpace) input) [] Nothing of
                Nothing -> Left "Error: malformed expression"
                Just v -> Right v
  where -- string to parse -> list of prev exprs -> previous operation (constructor) -> resulting expression
        go :: String -> [Maybe Expr] -> Maybe (Expr -> Expr -> Expr) -> Maybe Expr
        go "" [m] Nothing = m
        go "" [n, m] (Just op) = op <$> m <*> n
        go "" _ _ = Nothing
        go ('+':xs) (n:m:nums) (Just op) = go xs ((op <$> m <*> n) : nums) (Just Add)
        go ('+':xs) [m] Nothing = let (rest, y) = takeNumber xs
                                  in go rest [fmap I y, m] (Just Add)
        go ('-':xs) (n:m:nums) (Just op) = go xs ((op <$> m <*> n) : nums) (Just Sub)
        go ('-':xs) [m] Nothing = let (rest, y) = takeNumber xs
                                  in go rest [fmap I y, m] (Just Sub)
        go ('*':xs) (m:nums) op = let (rest, y) = takeNumber xs
                                  in go rest ((Mul <$> m <*> fmap I y) : nums) op
        go ('/':xs) (m:nums) op = let (rest, y) = takeNumber xs
                                  in go rest ((Div <$> m <*> fmap I y) : nums) op
        go s nums op = let (rest, n) = takeNumber s
                       in go rest ((fmap I n) : nums) op

        -- take next number
        takeNumber :: String -> (String, Maybe Double)
        takeNumber ('-':s) = fmap (fmap (read . ('-':))) $ helper s
        takeNumber s = fmap (fmap read) $ helper s
        helper (x:_) | x `notElem` "0123456789" = ("", Nothing)
        helper s = fmap Just . swap . break (`notElem` "0123456789") $ s
        
