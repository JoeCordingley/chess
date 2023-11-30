{-# LANGUAGE NoImplicitPrelude #-}
module Game
  ( start,
  )
where

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
  deriving (Eq, Ord)

--  | B
--  | C
--  | D
--  | E
--  | F
--  | G
--  | H

data Rank
  = One
  deriving (Eq, Ord)
--  | Two
--  | Three
--  | Four
--  | Five
--  | Six
--  | Seven
--  | Eight

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
knightMoves = undefined

bishopLines :: Space -> [[Space]]
bishopLines = undefined

queenLines :: Space -> [[Space]]
queenLines space = bishopLines space ++ rookLines space

rookLines :: Space -> [[Space]]
rookLines = undefined

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
