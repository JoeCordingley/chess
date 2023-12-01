{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}
module Game
where

import qualified Data.Map as Map
import Data.Map (Map, (!))
import Relude

data Player
  = White
  | Black
  deriving (Show, Eq, Ord)

data Result
  = Winner Player
  | Draw

data Status
  = Playing Board
  | Finished Result

data EndStatus
  = Checkmate
  | Stalemate

data Board = Board {pieceMap :: Map Space Piece}

-- = Space -> Maybe Piece

newtype MovesExtendingInASingleDirection 
 = MovesExtendingInASingleDirection [Space]

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

data Move

data AttackingMove = AttackingMove {pieceUnderAttack :: PieceType }

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
  Nothing -> Playing board
  where
    finishedStatus = if noLegalMoves then Just (if kingIsUnderAttack then Checkmate else Stalemate) else Nothing
      where
        noLegalMoves = null $ legalMoves (other player) board
        kingIsUnderAttack = King `elem` piecesUnderAttack 
          where
            piecesUnderAttack = pieceUnderAttack <$> attackingMoves where
              attackingMoves = pieces player board >>= uncurry linesOfAttack >>= (maybeToList . attackingMoveOnBoard) where
                attackingMoveOnBoard spaces = AttackingMove <$> (firstJust (pieceAt board) spaces >>= enemyPiece player)
              
legalMoves :: Player -> Board -> [Move]
legalMoves = undefined

linesOfAttack :: Piece -> Space -> [[Space]]
linesOfAttack (Piece player pieceType) = case pieceType of
   Pawn -> map (:[]) . pawnAttacks player
   Knight -> map (:[]) . knightMoves
   Bishop -> bishopLines
   Rook -> rookLines
   King -> map (:[]) . kingMoves
   Queen -> queenLines

pawnAttacks :: Player -> Space -> [Space]
pawnAttacks = undefined

knightMoves :: Space -> [Space]
knightMoves space = filter (knightMove space) spaces where
  knightMove (f1, r1) (f2, r2) = diffTuple == (1, 2) || diffTuple == (2, 1) where
    diffTuple = (f1 `diff` f2, r1 `diff` r2) where
      diff x y = abs $ fromEnum x - fromEnum y

spaces :: [Space]
spaces = (,) <$> files <*> ranks

ranks :: [Rank]
ranks = [One, Two, Three, Four, Five, Six, Seven, Eight]

files :: [File]
files = [A, B, C, D, E, F, G, H]

bishopLines :: Space -> [[Space]]
bishopLines = undefined

queenLines :: Space -> [[Space]]
queenLines space = bishopLines space ++ rookLines space

rookLines :: Space -> [[Space]]
rookLines space = map (lineExtendingFrom space) orthogonalDirections where
  lineExtendingFrom (f, r) (fd, rd) = lextendingFrom f fd `zip` lextendingFrom r rd
  orthogonalDirections = map (,EQ) ltgt ++ map (EQ,) ltgt where
    ltgt = [LT, GT]

lextendingFrom :: (Enum a, Bounded a) => a -> Ordering -> [a]
lextendingFrom a GT = [a..]
lextendingFrom a LT = map toEnum [i, i - 1 .. (fromEnum (maxBound `asTypeOf` a))] where
  i = fromEnum a
lextendingFrom a EQ = repeat a

kingMoves :: Space -> [Space]
kingMoves = undefined

pieces :: Player -> Board -> [(Piece, Space)]
pieces = undefined

other :: Player -> Player
other White = Black
other Black = White

pieceAt :: Board -> Space -> Maybe Piece
pieceAt board space = Map.lookup space (pieceMap board)

enemyPiece :: Player -> Piece -> Maybe PieceType
enemyPiece player (Piece otherPlayer pieceType)
  | otherPlayer == other player = Just pieceType
  | otherwise = Nothing

applyMove :: Board -> Move -> Board
applyMove = undefined


play :: Monad f => GetMove f -> f Result
play getMove = play' White startingBoard where
  play' player board = do
    newState <- playMove player board
    case newState of
      Playing newBoard -> play' (nextPlayer player) newBoard
      Finished result -> return result
  playMove player board = postMoveStatus player <$> newBoard 
    where
      newBoard = applyMove board <$> getMove player board
  nextPlayer = other

firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust f = getFirst . foldMap (First . f)
