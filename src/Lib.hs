module Lib
where

import Relude
import Prelude (read, interact)
import Data.Char (isDigit)

type Parser = StateT String []

parse :: Parser a -> String -> [(a, String)]
parse = runStateT 

eos :: Parser () 
eos = get >>= guard . null

anyChar :: Parser Char
anyChar = StateT $ maybeToList . uncons

ifChar :: (Char -> Bool) -> Parser Char
ifChar p = anyChar >>= lift . guarded p

char :: Char -> Parser Char
char = ifChar . (==)

digit :: Parser Char
digit = ifChar isDigit

nat :: Read a => Parser a
nat = read <$> some digit

int :: (Num a, Read a) => Parser a
int = neg <|> nat

neg :: (Num a, Read a) => Parser a
neg = char '-' *> (negate <$> nat)

whitespace :: Parser String
whitespace = many $ char ' ' 

token :: Parser a -> Parser a
token p = whitespace *> p <* whitespace

expr :: (Fractional a, Eq a, Read a) => Parser (Maybe a)
expr = chainl term fun where
  fun = minus <|> plus
  minus = liftA2 (-) <$ (token . char) '-'
  plus = liftA2 (+) <$ (token . char) '+'

chainl :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl p op = p >>= rest where
  rest x = do
    f <- op
    y <- p
    rest $ f x y
    <|> return x

term :: (Fractional a, Eq a, Read a) => Parser (Maybe a)
term = chainl factor $ liftA2 (*) <$ (token . char) '*' <|> divMaybe' <$ (token . char) '/' where
  divMaybe' x y = join $ divMaybe <$> x <*> y 

factor :: (Fractional a, Eq a, Read a) => Parser (Maybe a)
factor = (token . char) '(' *> expr <* (token . char) ')' <|> fmap Just int

someFunc :: IO ()
someFunc = interact $ show . parse expr 

divMaybe :: (Fractional a, Eq a, Read a) => a -> a -> Maybe a 
divMaybe x y = if y /= 0 then Just $ x / y else Nothing
  
--pow :: Int -> Int -> Int
--pow x 0 = 1
--pow x n | n `mod` 2 == 0 = y * y where
--  y = pow x (n `div` 2)
--pow x n = x * pow x (n - 1)

pow = pow' 1 where
  pow' a x 0 = a
  pow' a x n | n `mod` 2 == 0 = pow' a (x*x) (n `div` 2)
             | otherwise = pow' (a*x) x (n - 1)

--times :: Int -> Int -> Int
--times a 0 = 0
--times a n | n `mod` 2 == 0 = double a + times a (halve n) 
--          | otherwise = a + times a (n - 1)
--  where
--    double a = a + a
--    halve a = a `div` 2

times :: Int -> Int -> Int
times = times' 0 where
  times' a _ 0 = a
  times' a x y | y `mod` 2 == 0 = times' a (double x) (halve y)
               | otherwise = times' (a + x) x (y - 1)
--divAndR :: Int -> Int -> (Int, Int)
--divAndR n d = 
--    if d > n 
--      then (0, n) 
--      else if n `mod` 2 == 0 
--        then let (m, r) = divAndR (halve n) d in 
--          (double m, double r) 
--        else 
--          let (m, r) = divAndR (n-d) d in
--            (1+m, r) 
--divAndR :: Int -> Int -> (Int, Int)
--divAndR = div' 0 0 where
--  div' a r n d = 
--    if a > d then div' (a - d) (r+1) n d
--    else if n == 0 (a, r)
--    else if n `mod` 2 == 0 div' (2*a+1) (2*r) (halve n) d
--    else div' (a+1) (2*r) (n-1)


double a = a + a
halve a = a `div` 2

fib :: Int -> Int
fib = fib' 0 1 1 0 where
  fib' p q a b n = if n == 0 then b
    else if n `mod` 2 == 0 then fib' (p*p + q*q) (q*q + double(p*q)) a b (halve n)
    else fib' p q (b*q+a*q+a*p) (b*p+a*q) (n-1)
    
