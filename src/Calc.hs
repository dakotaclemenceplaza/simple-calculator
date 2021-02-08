module Calc where

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
        parseInput s = helper1 $ break (=='=') s    -- see if it has an assignment operator
        helper1 (var, expr) | not (null var) &&                                                -- assignment branch
                             all (`elem` ['a'..'z']) var &&
                             not (null expr) = case go (tail expr) [] Nothing of
                                                 Nothing -> Left "Error: malformed expression"
                                                 Just v -> Right $ Assign var v
        helper1 (expr, rest) | null rest = case go expr [] Nothing of                    -- expression without assignment
                                           Nothing -> Left "Error: malformed expression"
                                           Just v -> Right $ Eval v
        helper1 _ = Left "Error: malformed expression"

        -- parsing of expression
        -- string to parse -> list of prev exprs -> previous operation (constructor) -> resulting expression
        go :: String -> [Maybe Expr] -> Maybe (Expr -> Expr -> Expr) -> Maybe Expr
        go "" [m] Nothing = m
        go "" [n, m] (Just op) = op <$> m <*> n
        go "" _ _ = Nothing
        go ('+':xs) (n:m:nums) (Just op) = go xs ((op <$> m <*> n) : nums) (Just Add)
        go ('+':xs) [m] Nothing = let (rest, y) = takeNext xs
                                  in go rest [y, m] (Just Add)
        go ('-':xs) (n:m:nums) (Just op) = go xs ((op <$> m <*> n) : nums) (Just Sub)
        go ('-':xs) [m] Nothing = let (rest, y) = takeNext xs
                                  in go rest [y, m] (Just Sub)
        go ('*':xs) (m:nums) op = let (rest, y) = takeNext xs
                                  in go rest ((Mul <$> m <*> y) : nums) op
        go ('/':xs) (m:nums) op = let (rest, y) = takeNext xs
                                  in go rest ((Div <$> m <*> y) : nums) op
        go s nums op = let (rest, n) = takeNext s
                       in go rest (n : nums) op

        -- take next number or variable
        takeNext :: String -> (String, Maybe Expr)
        takeNext ('-':s) = fmap (fmap (I . read . ('-':))) $ number s
        takeNext s = case number s of
          (_, Nothing) -> fmap (fmap Var) . var $ s
          v -> fmap (fmap (I . read)) v
        number (x:_) | x `notElem` "0123456789" = ("", Nothing)
        number s = fmap Just . swap . break (`notElem` "0123456789") $ s
        var (x:_) | x `notElem` ['a'..'z'] = ("", Nothing)
        var s = fmap Just . swap . break (`notElem` ['a'..'z']) $ s
