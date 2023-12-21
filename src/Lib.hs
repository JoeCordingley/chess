module Lib
where

import Relude
import Prelude (read)
import Data.Char (isDigit)

type Parser = StateT String Maybe

parse :: Parser a -> String -> Maybe a
parse parser s = do
  (a, remaining) <- runStateT parser s 
  pureIf (null remaining) a 

pureIf :: Alternative f => Bool -> a -> f a
pureIf True = pure
pureIf False = const empty

char :: Parser Char
char = StateT uncons

ifChar :: (Char -> Bool) -> Parser Char
ifChar p = char >>= lift . guarded p

thisChar :: Char -> Parser Char
thisChar = ifChar . (==)

digit :: Parser Char
digit = ifChar isDigit

nat :: Parser Int
nat = read <$> some digit

int :: Parser Int
int = thisChar '-' *> (negate <$> nat) <|> nat


someFunc :: IO ()
someFunc = putStrLn "someFunc"
