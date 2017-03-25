module Chess.Theory where

import Chess
import Chess.Board
import Chess.Move
import Chess.Position
import Chess.Rules

position1e4e5 = (apply (PieceMovement e7 e5 Nothing) . apply (PieceMovement e2 e4 Nothing)) initialPosition

positionBerlin = foldl (flip apply) initialPosition [
  PieceMovement e2 e4 Nothing,
  PieceMovement e7 e5 Nothing,
  PieceMovement g1 f3 Nothing,
  PieceMovement b8 c6 Nothing,
  PieceMovement f1 b5 Nothing,
  PieceMovement g8 f6 Nothing
  ]


