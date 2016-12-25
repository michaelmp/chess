module Chess.Move where

import qualified Data.Char as C

import Chess
import Chess.Board
import Chess.Position

type MovementPattern = AlgebraicSquare -> AlgebraicSquare

isOrtho :: AlgebraicSquare -> AlgebraicSquare -> Bool
isOrtho origin destination = sameRank || sameFile where
  sameRank = rank origin == rank destination
  sameFile = file origin == file destination

up :: MovementPattern
up (AlgebraicSquare file rank) = AlgebraicSquare file' rank' where
  file' = file
  rank' = rank + 1 

upRight :: MovementPattern
upRight (AlgebraicSquare file rank) = AlgebraicSquare file' rank' where
  file' = C.chr $ C.ord file + 1
  rank' = rank + 1 

right :: MovementPattern
right (AlgebraicSquare file rank) = AlgebraicSquare file' rank' where
  file' = C.chr $ C.ord file + 1
  rank' = rank 

downRight :: MovementPattern
downRight (AlgebraicSquare file rank) = AlgebraicSquare file' rank' where
  file' = C.chr $ C.ord file + 1
  rank' = rank - 1 

down :: MovementPattern
down (AlgebraicSquare file rank) = AlgebraicSquare file' rank' where
  file' = file
  rank' = rank - 1 

downLeft :: MovementPattern
downLeft (AlgebraicSquare file rank) = AlgebraicSquare file' rank' where
  file' = C.chr $ C.ord file - 1
  rank' = rank - 1 

left :: MovementPattern
left (AlgebraicSquare file rank) = AlgebraicSquare file' rank' where
  file' = C.chr $ C.ord file - 1
  rank' = rank 

upLeft :: MovementPattern
upLeft (AlgebraicSquare file rank) = AlgebraicSquare file' rank' where
  file' = C.chr $ C.ord file - 1
  rank' = rank + 1 

slide :: MovementPattern -> [MovementPattern]
slide m = drop 1 $ iterate (. m) id

kingCastleShort :: MovementPattern
kingCastleShort = right . right

kingCastleLong :: MovementPattern
kingCastleLong = left . left 

rookCastleShort :: MovementPattern
rookCastleShort = left . left

rookCastleLong :: MovementPattern
rookCastleLong = right . right . right

pawnDoubleAdvance :: Side -> MovementPattern
pawnDoubleAdvance side = advance . advance where
  advance = pawnAdvance side

pawnAdvance :: Side -> MovementPattern
pawnAdvance side = case side of
  White -> up
  Black -> down

pawnCaptureRight :: Side -> MovementPattern
pawnCaptureRight side = case side of
  White -> up . right
  Black -> down . right

pawnCaptureLeft :: Side -> MovementPattern
pawnCaptureLeft side = case side of
  White -> up . left
  Black -> down . left

-- Infinite movements for sliders.
pieceMovementPatternPatterns :: Side -> Piece -> [[MovementPattern]]
pieceMovementPatternPatterns side piece = case piece of
  King   -> fmap (:[]) [
              up,
              upRight,
              right,
              downRight,
              down,
              downLeft,
              left,
              upLeft,
              kingCastleShort,
              kingCastleLong
            ]
  Queen  -> [
              slide up,
              slide upRight,
              slide right,
              slide downRight,
              slide down,
              slide downLeft,
              slide left,
              slide upLeft
            ]
  Rook   -> [
              slide up,
              slide right,
              slide down,
              slide left
            ] ++ fmap (:[]) [
              rookCastleShort,
              rookCastleLong
            ]
  Bishop -> [
              slide upRight,
              slide downRight,
              slide downLeft,
              slide upLeft
            ]
  Knight -> fmap (:[]) [
              up . up . right,
              up . right . right,
              down . right . right,
              down . down . right,
              down . down . left,
              down . left . left,
              up . left . left,
              up . up . left
            ]
  Pawn   -> fmap (:[]) [
              pawnAdvance side,
              pawnDoubleAdvance side,
              pawnCaptureRight side,
              pawnCaptureLeft side
            ]

-- Finite.
-- All bounded movements from an origin square.
boundedMoves :: Side -> Piece -> AlgebraicSquare -> [MovementPattern]
boundedMoves side piece origin = restrictedMoves side piece origin onBoard

restrictedMoves :: Side -> Piece -> AlgebraicSquare -> (AlgebraicSquare -> Bool) -> [MovementPattern]
restrictedMoves side piece origin f = concatMap restrict patterns where
  patterns = pieceMovementPatternPatterns side piece
  restrict = takeWhile (\move -> f $ move origin)

type Origin = AlgebraicSquare
type Destination = AlgebraicSquare
type PromotionPiece = Piece

data Move = CastleShort | CastleLong | PieceCapture Origin Destination (Maybe PromotionPiece) | PieceMovement Origin Destination (Maybe PromotionPiece)

