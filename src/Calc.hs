module Calc (calc, eval, parse) where

import System.IO (hFlush, stdout)
import Data.Char (isSpace)
import Data.Tuple (swap)
import Control.Monad.State (StateT, runStateT, get, modify, lift)

calc = run []

run s = do
  putStr "> "
  hFlush stdout
  input <- getLine
  case parse input of
    Left e -> print e >> run s
    Right i -> case runStateT (eval i) s of
                 Left e -> print e >> run s
                 Right (v, newS) -> do
                   putStrLn . cleanDouble $ v
                   run newS

data Input = Assign String Expr | Eval Expr
  
data Expr = I Double
          | Var String
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr

eval :: Input -> StateT [(String, Double)] (Either String) Double
eval (Assign var expr) = do
 v <- go expr
 modify $ addVar (var, v)
 pure v
eval (Eval expr) = go expr

-- helper for eval
go :: Expr -> StateT [(String, Double)] (Either String) Double
go (Add x y) = (+) <$> go x <*> go y
go (Sub x y) = (-) <$> go x <*> go y
go (Mul x y) = (*) <$> go x <*> go y
go (Div x y) = (/) <$> go x <*> go y
go (I x) = pure x
go (Var var) = do
  s <- get
  case lookup var s of
    Just v -> pure v
    Nothing -> lift $ Left $ "Error: did not found variable " <> var

cleanDouble = reverse . dropWhile (`elem` "0.") . reverse . show

addVar (variable, value) s = (variable, value) : del variable s
  where del _ [] = []
        del var ((v,_):xs) | v == var = xs
        del var (v:xs) = v : del var xs

-- Not the best handling of errors
parse :: String -> Either String Input
parse = parseInput . filter (not . isSpace)
  where parseInput s | null s = Left "Empty input"
        parseInput s = helper $ break (=='=') s    -- see if it has an assignment operator
        helper (var, expr) | not (null var) &&                                              -- assignment branch
                             all (`elem` ['a'..'z']) var &&
                             not (null expr) = case parseE (tail expr) [] Nothing of
                                                 Nothing -> Left "Error: malformed expression"
                                                 Just v -> Right $ Assign var v
        helper (expr, rest) | null rest = case parseE expr [] Nothing of                    -- expression without assignment
                                           Nothing -> Left "Error: malformed expression"
                                           Just v -> Right $ Eval v
        helper _ = Left "Error: malformed expression"

-- helper for parse - parsing of expression
-- string to parse -> list of prev exprs -> previous operation (constructor) -> resulting expression
parseE :: String -> [Maybe Expr] -> Maybe (Expr -> Expr -> Expr) -> Maybe Expr
parseE "" [m] Nothing = m
parseE "" [n, m] (Just op) = op <$> m <*> n
parseE "" _ _ = Nothing
parseE ('+':xs) (n:m:nums) (Just op) = parseE xs ((op <$> m <*> n) : nums) (Just Add)
parseE ('+':xs) [m] Nothing = let (rest, y) = takeNext xs
                              in parseE rest [y, m] (Just Add)
parseE ('-':xs) (n:m:nums) (Just op) = parseE xs ((op <$> m <*> n) : nums) (Just Sub)
parseE ('-':xs) [m] Nothing = let (rest, y) = takeNext xs
                              in parseE rest [y, m] (Just Sub)
parseE ('*':xs) (m:nums) op = let (rest, y) = takeNext xs
                              in parseE rest ((Mul <$> m <*> y) : nums) op
parseE ('/':xs) (m:nums) op = let (rest, y) = takeNext xs
                              in parseE rest ((Div <$> m <*> y) : nums) op
parseE s nums op = let (rest, n) = takeNext s
                   in parseE rest (n : nums) op

-- take next number or variable
takeNext :: String -> (String, Maybe Expr)
takeNext ('-':s) = fmap (fmap (I . read . ('-':))) $ number s
takeNext s = case number s of
  (_, Nothing) -> fmap (fmap Var) . variable $ s
  v -> fmap (fmap (I . read)) v

number (x:_) | x `notElem` "0123456789" = ("", Nothing)
number s = fmap Just . swap . break (`notElem` "0123456789") $ s

variable (x:_) | x `notElem` ['a'..'z'] = ("", Nothing)
variable s = fmap Just . swap . break (`notElem` ['a'..'z']) $ s
