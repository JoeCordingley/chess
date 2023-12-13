{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Game where

import Data.List (singleton)
import qualified Data.Map as Map
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

data Board = Board {pieceMap :: Map Space Piece}

-- = Space -> Maybe Piece

data Piece
  = Piece Player PieceType
  deriving (Eq, Ord)

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
  deriving (Eq, Ord, Enum, Bounded, Show)

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

data Move = AttackingMove AttackingMoveData | NonAttackingMove NonAttackingMoveData

data NonAttackingMoveData = NonAttackingMoveData Space Space

data AttackingMoveData = AttackingMoveData {pieceUnderAttack :: PieceType}

startingBoard :: Board
startingBoard = undefined

start :: IO ()
start = play getMoveCli >>= printResult

printResult :: Result -> IO ()
printResult = putStrLn . resultString

resultString :: Result -> String
resultString (Winner player) = (show player) ++ " wins!"
resultString Draw = "It's a draw"

type GetMove f = Player -> Board -> f Move

data PlacedPiece = PlacedPiece Piece Space deriving (Eq, Ord)

getMoveCli :: GetMove IO
getMoveCli = undefined

postMoveStatus :: Player -> Board -> Status
postMoveStatus player board = case finishedStatus of
  Just Checkmate -> Finished (Winner player)
  Just Stalemate -> Finished Draw
  Nothing -> Playing
  where
    finishedStatus = if noLegalMoves then Just (if kingIsUnderAttack then Checkmate else Stalemate) else Nothing
      where
        noLegalMoves = null $ legalMoves (other player) board
        kingIsUnderAttack = King `elem` piecesUnderAttack
          where
            piecesUnderAttack = pieceUnderAttack <$> attackingMoves player board where

legalMoves :: Player -> Board -> [Move]
legalMoves player board = (map AttackingMove (attackingMoves player board) ++ map NonAttackingMove (nonAttackingMoves player board))

nonAttackingMoves :: Player -> Board -> [NonAttackingMoveData]
nonAttackingMoves player board = do
  (piece, fromSpace) <- pieces player board
  lineOfMovement <- linesOfMovement piece fromSpace
  toSpace <- takeWhile unoccupied lineOfMovement
  return $ NonAttackingMoveData fromSpace toSpace
  where
    unoccupied space = isNothing $ pieceAt board space

linesOfMovement :: Piece -> Space -> [[Space]]
linesOfMovement (Piece player pieceType) = case pieceType of
  Pawn -> singleton . pawnMoves player
  Knight -> map singleton . knightMoves
  Bishop -> bishopLines
  Rook -> rookLines
  King -> map singleton . kingMoves
  Queen -> queenLines

attackingMoves player board = pieces player board >>= uncurry linesOfAttack >>= (maybeToList . attackingMoveOnBoard)
  where
    attackingMoveOnBoard spaces = AttackingMoveData <$> (firstJust (pieceAt board) spaces >>= enemyPiece player)

linesOfAttack :: Piece -> Space -> [[Space]]
linesOfAttack (Piece player Pawn) = map singleton . pawnAttacks player
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
          where
            rankDiff = fromEnum r2 - fromEnum r1
        movementIsOneToTheSide = absFileDiff == 1
          where
            absFileDiff = abs $ fromEnum f2 - fromEnum f1

knightMoves :: Space -> [Space]
knightMoves space = filter (knightMove space) allSpaces
  where
    knightMove x y = distances' == (1, 2) || distances' == (2, 1)
      where
        distances' = distances x y

distances (f1, r1) (f2, r2) = (f1 `distance` f2, r1 `distance` r2)
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

pieces :: Player -> Board -> [(Piece, Space)]
pieces player board = do
  space <- allSpaces
  piece <- maybeToList $ Map.lookup space $ pieceMap board
  return (piece, space)

other :: Player -> Player
other White = Black
other Black = White

pieceAt :: Board -> Space -> Maybe Piece
pieceAt board space = Map.lookup space (pieceMap board)

enemyPiece :: Player -> Piece -> Maybe PieceType
enemyPiece ofPlayer (Piece player pieceType)
  | player == other ofPlayer = Just pieceType
  | otherwise = Nothing

applyMove :: Board -> Move -> Board
applyMove board (NonAttackingMove values) = undefined
applyMove board (AttackingMove values) = undefined

play :: Monad f => GetMove f -> f Result
play getMove = play' White startingBoard
  where
    play' player board = do
      (status, newBoard) <- playMove player
      case status of
        Playing -> play' (nextPlayer player) newBoard
        Finished result -> return result
      where
        playMove player = etc <$> getMove player board
          where
            etc move = (status, newBoard)
              where
                newBoard = applyMove board move
                status = postMoveStatus player $ newBoard
        nextPlayer = other

firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust f = getFirst . foldMap (First . f)
