{-# LANGUAGE NoImplicitPrelude #-}

module Cli (main) where

import Relude hiding (readFile)
import Game (play, GetMove, Board, Result(..), Player(..), PieceType, Space, Rank(..), File(..), Move(..), legalMoves, PieceType(..), ranks, files)
import Data.Map (lookup)

main :: IO ()
main = play getMoveCli >>= printResult

getMoveCli :: GetMove IO
getMoveCli player board = do
  printBoard board
  prompt (readValidMove player board) "not a valid move"

interleave :: a -> [a] -> [a]
interleave _ [] = []
interleave _ [y] = [y]
interleave x (y:ys) = y : x : interleave x ys

printBoard :: Board -> IO ()
printBoard board = traverse_ printRank $ reverse ranks where
  printRank rank = putStrLn $ interleave ' ' $ concatMap (printSpace rank) files 
  printSpace rank file = fromMaybe "." . fmap pieceString $ lookup (file, rank) board 
  pieceString (White, King) = "K"
  pieceString (White, Queen) = "Q"
  pieceString (White, Rook) = "R"
  pieceString (White, Bishop) = "B"
  pieceString (White, Knight) = "N"
  pieceString (White, Pawn) = "P"
  pieceString (Black, King) = "k"
  pieceString (Black, Queen) = "q"
  pieceString (Black, Rook) = "r"
  pieceString (Black, Bishop) = "b"
  pieceString (Black, Knight) = "n"
  pieceString (Black, Pawn) = "p"
-- pieceString (White, King) = "♔"
-- pieceString (White, Queen) = "♕"
-- pieceString (White, Rook) = "♖"
-- pieceString (White, Bishop) = "♗"
-- pieceString (White, Knight) = "♘"
-- pieceString (White, Pawn) = "♙"
-- pieceString (Black, King) = "♚"
-- pieceString (Black, Queen) = "♛"
-- pieceString (Black, Rook) = "♜"
-- pieceString (Black, Bishop) = "♝"
-- pieceString (Black, Knight) = "♞"
-- pieceString (Black, Pawn) = "♟︎"

printResult :: Result -> IO ()
printResult = putStrLn . resultString

prompt :: (Text -> Maybe a) -> String -> IO a
prompt f msg = untilJustM $ getLine >>= ifNothing (putStrLn msg) . f 

untilJustM :: Monad f => f (Maybe a) -> f a
untilJustM fa = fa >>= maybe (untilJustM fa) pure

readValidMove :: Player -> Board -> Text -> Maybe Move
readValidMove player board = singletonMaybe . (readMove . toString >=> disambiguate) where
  disambiguate writtenMove = filter (isMove writtenMove) $ legalMoves player board where
    isMove (pieceType, maybeOriginFile, maybeOriginRank, isCapture', destination) move = 
      movePiece move == pieceType && 
      isCapture' == (isCapture move) && 
      toSpace move == destination &&
      all ((==) (fst . fromSpace $ move)) maybeOriginFile  &&
      all ((==) (snd . fromSpace $ move)) maybeOriginRank 

readMove :: String -> [(PieceType, Maybe File, Maybe Rank, Bool, Space)]
readMove text = evalStateT parse text where
  parse = (,,,,) <$> parsePiece <*> parseFileMaybe <*> parseRankMaybe <*> parseCapture <*> parseSpace <* eos
  parseRankMaybe = parseMaybe parseRank
  parseCapture = True <$ char 'x' <|> return False
  parseFileMaybe = parseMaybe parseFile
  parseSpace = (,) <$> parseFile <*> parseRank
  parsePiece = collectChar readPiece <|> return Pawn
  parseFile = collectChar readFile
  parseRank = collectChar readRank

collectChar :: (Char -> Maybe b) -> Parser b
collectChar f = anyChar >>= lift . maybeToList . f

char :: Char -> Parser Char
char = ifChar . (==)

anyChar :: Parser Char
anyChar = StateT $ maybeToList . uncons

ifChar :: (Char -> Bool) -> Parser Char
ifChar p = anyChar >>= lift . guarded p

readPiece :: Char -> Maybe PieceType
readPiece c = case c of
  'K' -> Just King
  'Q' -> Just Queen
  'R' -> Just Rook
  'N' -> Just Knight
  'B' -> Just Bishop
  _  -> Nothing

readRank :: Char -> Maybe Rank
readRank c = case c of
  '1' -> Just One
  '2' -> Just Two
  '3' -> Just Three
  '4' -> Just Four
  '5' -> Just Five
  '6' -> Just Six
  '7' -> Just Seven
  '8' -> Just Eight
  _ -> Nothing

readFile :: Char -> Maybe File
readFile c = case c of
  'a' -> Just A
  'b' -> Just B
  'c' -> Just C
  'd' -> Just D
  'e' -> Just E
  'f' -> Just F
  'g' -> Just G
  'h' -> Just H
  _ -> Nothing

type Parser = StateT String []

eos :: Parser () 
eos = void $ get >>= guarded null

parseMaybe :: Parser a -> Parser (Maybe a)
parseMaybe s = Just <$> s <|> return Nothing

singletonMaybe :: [a] -> Maybe a
singletonMaybe [a] = Just a
singletonMaybe _ = Nothing

resultString :: Result -> String
resultString (Winner player) = (show player) ++ " wins!"
resultString Draw = "It's a draw"

ifNothing :: Applicative f => f b -> Maybe a -> f (Maybe a)
ifNothing f = maybe (Nothing <$ f) (pure . Just)
