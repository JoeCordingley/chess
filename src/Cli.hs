{-# LANGUAGE NoImplicitPrelude #-}

module Cli where

import Relude hiding (readFile)
import Game (play, GetMove, Board, Result(..), Player, PieceType, Space, Rank(..), File(..), Move(..), legalMoves, PieceType(..))

start :: IO ()
start = play getMoveCli >>= printResult

getMoveCli :: GetMove IO
getMoveCli player board = do
  putStrLn . show $ printedBoard board
  prompt (readValidMove player board) "not a valid move"

printedBoard :: Board -> Text
printedBoard = undefined

printResult :: Result -> IO ()
printResult = putStrLn . resultString

prompt :: (Text -> Maybe a) -> String -> IO a
prompt f error = untilJustM $ getLine >>= ifNothing (putStrLn error) . f 

untilJustM :: Monad f => f (Maybe a) -> f a
untilJustM fa = fa >>= maybe (untilJustM fa) pure

readValidMove :: Player -> Board -> Text -> Maybe Move
readValidMove player board = readMove . toString >=> disambiguate where
  disambiguate writtenMove = singletonMaybe . filter (isMove writtenMove) $ legalMoves player board where
    isMove (pieceType, maybeOriginFile, maybeOriginRank, isCapture', destination) move = 
      movePiece move == pieceType && 
      isCapture' == (isCapture move) && 
      toSpace move == destination &&
      all ((==) (fst . fromSpace $ move)) maybeOriginFile  &&
      all ((==) (snd . fromSpace $ move)) maybeOriginRank 

readMove :: String -> Maybe (PieceType, Maybe File, Maybe Rank, Bool, Space)
readMove text = evalStateT parse text where
  parse = (,,,,) <$> parsePiece <*> parseFileMaybe <*> parseRankMaybe <*> parseCapture <*> parseSpace <* eos
  parsePiece = collectChar readPiece <|> return Pawn
  parseFile = collectChar readFile
  parseRank = collectChar readRank
  parseRankMaybe = parseMaybe parseRank
  parseFileMaybe = parseMaybe parseFile
  parseSpace = (,) <$> parseFile <*> parseRank
  parseCapture = True <$ char 'x' <|> return False

collectChar :: (Char -> Maybe a) -> Parser a
collectChar f = anyChar >>= lift . f

char :: Char -> Parser Char
char = ifChar . (==)

anyChar :: Parser Char
anyChar = StateT uncons

ifChar :: (Char -> Bool) -> Parser Char
ifChar p = anyChar >>= lift . guarded p

readPiece :: Char -> Maybe PieceType
readPiece char = case char of
  'K' -> Just King
  'Q' -> Just Queen
  'R' -> Just Rook
  'N' -> Just Knight
  'B' -> Just Bishop
  _  -> Nothing

readRank :: Char -> Maybe Rank
readRank char = case char of
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
readFile char = case char of
  'A' -> Just A
  'B' -> Just B
  'C' -> Just C
  'D' -> Just D
  'E' -> Just E
  'F' -> Just F
  'G' -> Just G
  'H' -> Just H
  _ -> Nothing

type Parser = StateT String Maybe

eos :: Parser () 
eos = void $ get >>= guarded null

parseMaybe :: Parser a -> Parser (Maybe a)
parseMaybe s = Just <$> s <|> return Nothing

singletonMaybe [a] = Just a
singletonMaybe _ = Nothing

resultString :: Result -> String
resultString (Winner player) = (show player) ++ " wins!"
resultString Draw = "It's a draw"

ifNothing :: Applicative f => f b -> Maybe a -> f (Maybe a)
ifNothing f = maybe (Nothing <$ f) (pure . Just)
