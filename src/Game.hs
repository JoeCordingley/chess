{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

module Game (play, GetMove, Board, Result(..), Player, Space, Rank(..), File(..), Move(..), legalMoves, PieceType(..)) where

import Data.List (singleton)
import qualified Data.Map as Map
import Data.Map ((!))
import Relude

data Player
  = White
  | Black
  deriving (Show, Eq, Ord)

data Result
  = Winner Player
  | Draw

data Status
  = Playing
  | Finished Result

data EndStatus
  = Checkmate
  | Stalemate

type Board = Map Space Piece
 
type Piece
  = (Player, PieceType) 

data PieceType
  = King
  | Rook
  | Knight
  | Bishop
  | Queen
  | Pawn
  deriving (Eq, Ord)

type Space = (File, Rank)

data File
  = A
  | B
  | C
  | D
  | E
  | F
  | G
  | H
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

data Rank
  = One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  deriving (Eq, Ord, Enum, Bounded, Show)

data Move = Move {movePiece :: PieceType, fromSpace :: Space, toSpace :: Space, isCapture :: Bool} deriving Eq

data AttackingMove = AttackingMove {attackingPiece:: PieceType, pieceUnderAttack :: PieceType, attackingFrom :: Space, attackingTo :: Space}

startingBoard :: Board
startingBoard = Map.fromList $ do
  (rank, player, pieceTypes) <- [(One, White, otherPieces), (Two, White, pawns), (Seven, Black, pawns), (Eight, Black, otherPieces)]
  (file, pieceType) <- files `zip` pieceTypes
  return ((file, rank), (player, pieceType))
  where
    pawns = replicate 8 Pawn
    otherPieces = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]

type GetMove f = Player -> Board -> f Move

postMoveStatus :: Player -> Board -> Status
postMoveStatus player board = case finishedStatus of
  Just Checkmate -> Finished (Winner player)
  Just Stalemate -> Finished Draw
  Nothing -> Playing
  where
    finishedStatus = if noLegalMoves then Just (if kingIsUnderAttack then Checkmate else Stalemate) else Nothing
    noLegalMoves = null $ legalMoves (other player) board
    kingIsUnderAttack = King `elem` piecesUnderAttack
    piecesUnderAttack = pieceUnderAttack <$> attackingMoves player board 

legalMoves :: Player -> Board -> [Move]
legalMoves player board = (map toMove (attackingMoves player board) ++ nonAttackingMoves player board) where
  toMove attackingMove  = Move {movePiece = attackingPiece attackingMove, fromSpace = attackingFrom attackingMove, toSpace = attackingTo attackingMove, isCapture = True}

nonAttackingMoves :: Player -> Board -> [Move]
nonAttackingMoves player board = do
  (pieceType, fromSpace) <- pieces player board
  lineOfMovement <- linesOfMovement (player, pieceType) fromSpace
  toSpace <- takeWhile unoccupied lineOfMovement
  return $ Move {movePiece = pieceType, fromSpace = fromSpace, toSpace=toSpace, isCapture = False}
  where
    unoccupied space = isNothing $ pieceAt board space

linesOfMovement :: Piece -> Space -> [[Space]]
linesOfMovement (player, pieceType) = case pieceType of
  Pawn -> singleton . pawnMoves player
  Knight -> map singleton . knightMoves
  Bishop -> bishopLines
  Rook -> rookLines
  King -> map singleton . kingMoves
  Queen -> queenLines

attackingMoves :: Player -> Board -> [AttackingMove]
attackingMoves player board = do
  (pieceType, fromSpace) <- pieces player board 
  spaces <- linesOfAttack (player, pieceType) fromSpace
  maybeToList $ do
    (toSpace, piece) <- firstJust (fmapSnd $ pieceAt board) spaces
    attacking <- enemyPiece player piece
    return $ AttackingMove {attackingPiece = pieceType, pieceUnderAttack = attacking, attackingFrom = fromSpace, attackingTo = toSpace}
  where
    fmapSnd f a = (,) a <$> f a

linesOfAttack :: Piece -> Space -> [[Space]]
linesOfAttack (player, Pawn) = map singleton . pawnAttacks player
linesOfAttack piece = linesOfMovement piece

pawnMoves :: Player -> Space -> [Space]
pawnMoves = traverse . pawnRanks
  where
    pawnRanks White Two = [Three, Four]
    pawnRanks White rank = [succ rank]
    pawnRanks Black Seven = [Six, Five]
    pawnRanks Black rank = [pred rank]

pawnAttacks :: Player -> Space -> [Space]
pawnAttacks player space = filter (pawnAttack space) allSpaces
  where
    pawnAttack (f1, r1) (f2, r2) = movementIsOneForward && movementIsOneToTheSide
      where
        movementIsOneForward = (rankDiff == 1 && player == White) || (rankDiff == -1 && player == Black)
        rankDiff = fromEnum r2 - fromEnum r1
        movementIsOneToTheSide = absFileDiff == 1
        absFileDiff = abs $ fromEnum f2 - fromEnum f1

knightMoves :: Space -> [Space]
knightMoves space = filter (knightMove space) allSpaces
  where
    knightMove x y = distances' == (1, 2) || distances' == (2, 1)
      where
        distances' = distances x y

distances
  :: (Enum a, Enum b) =>
     (a, b) -> (a, b) -> (Int, Int)
distances (a1, b1) (a2, b2) = (a1 `distance` a2, b1 `distance` b2)
  where
    distance x y = abs $ fromEnum x - fromEnum y

bishopLines :: Space -> [[Space]]
bishopLines space = map (lineExtendingFrom space) diagonalDirections
  where
    diagonalDirections = (,) <$> np <*> np

queenLines :: Space -> [[Space]]
queenLines space = bishopLines space ++ rookLines space

rookLines :: Space -> [[Space]]
rookLines space = map (lineExtendingFrom space) orthogonalDirections
  where
    orthogonalDirections = map (,0) np ++ map (0,) np

kingMoves :: Space -> [Space]
kingMoves space = filter (kingMove space) allSpaces
  where
    kingMove x y = uncurry max (distances x y) == 1

lineExtendingFrom :: Space -> (Int, Int) -> [Space]
lineExtendingFrom (file, rank) (fileIncrement, rankIncrement) =
  enumFromByIncrement file fileIncrement
    `zip` enumFromByIncrement rank rankIncrement

np = [-1, 1]

enumFromByIncrement :: (Enum a, Bounded a) => a -> Int -> [a]
enumFromByIncrement a inc = map toEnum [init, init + inc .. fromEnum $ maxBound `asTypeOf` a]
  where
    init = fromEnum a

allSpaces :: [Space]
allSpaces = (,) <$> files <*> ranks

ranks :: [Rank]
ranks = [One, Two, Three, Four, Five, Six, Seven, Eight]

files :: [File]
files = [A, B, C, D, E, F, G, H]

pieces :: Player -> Board -> [(PieceType, Space)]
pieces player board = do
  space <- allSpaces
  (player', pieceType) <- maybeToList $ Map.lookup space board
  guard $ player' == player 
  return (pieceType, space)

other :: Player -> Player
other White = Black
other Black = White

pieceAt :: Board -> Space -> Maybe Piece
pieceAt board space = Map.lookup space board

enemyPiece :: Player -> Piece -> Maybe PieceType
enemyPiece ofPlayer (player, pieceType)
  | player == other ofPlayer = Just pieceType
  | otherwise = Nothing

applyMove :: Board -> Move -> Board
applyMove board move = Map.insert to (board ! from) (Map.delete from board) where
  to = toSpace move
  from = fromSpace move

play :: Monad f => GetMove f -> f Result
play getMove = play' White startingBoard where
  play' player board = do
    newBoard <- getBoard player board
    case postMoveStatus player newBoard of
      Playing -> play' (nextPlayer player) newBoard
      Finished result -> return result
  nextPlayer = other
  getBoard player board = applyMove board <$> getMove player board

firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust f = getFirst . foldMap (First . f)
