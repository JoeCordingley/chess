module Lib
where

import Relude
import Prelude (read, interact)
import Data.Char (isDigit)

type Parser = StateT String Maybe

parse :: Parser a -> String -> Maybe a
parse parser s = do
  (a, remaining) <- runStateT parser s 
  pureIf (null remaining) a 

pureIf :: Alternative f => Bool -> a -> f a
pureIf True = pure
pureIf False = const empty

anyChar :: Parser Char
anyChar = StateT uncons

ifChar :: (Char -> Bool) -> Parser Char
ifChar p = anyChar >>= lift . guarded p

char :: Char -> Parser Char
char = ifChar . (==)

digit :: Parser Char
digit = ifChar isDigit

nat :: Parser Int
nat = read <$> some digit

int :: Parser Int
int = token $ neg <|> nat

neg :: Parser Int
neg = char '-' *> (negate <$> nat)


whitespace :: Parser String
whitespace = many $ char ' ' 

token :: Parser a -> Parser a
token p = whitespace *> p <* whitespace

tokenChar :: Char -> Parser Char
tokenChar = token . char

expr :: Parser Int 
expr = (+) <$> term <* (token . char) '+' <*> expr <|> term where

term :: Parser Int
term = (*) <$> factor <* (token . char) '*' <*> term  <|> factor 

factor = (token . char) '(' *> expr <* (token . char) ')' <|> int

someFunc :: IO ()
someFunc = interact f where
  f s = show $ parse expr s
