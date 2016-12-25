module Chess.Rules where

import Chess
import Chess.Board
import Chess.Move
import Chess.Position

inCheck :: Side -> Position -> Bool
side `inCheck` position = and $ kingBoard side position `intersection` getCaptureBoard position side

-- Apply a piece capturing pattern to a board, accounting for occupied squares.
extendCaptures :: Side -> Piece -> Board -> Board -> Board
extendCaptures side piece pieceBoard occupiedBoard = case piece of
  Pawn -> (`fillSquares` emptyBoard) $ filter onBoard $ concatMap pseudoAttackedSquares pieceSquares where
    pieceSquares = squares pieceBoard
    pseudoAttackedSquares square = fmap ($ square) [pawnCaptureRight side, pawnCaptureLeft side]
  Rook -> (`fillSquares` emptyBoard) $ filter onBoard $ concatMap pseudoAttackedSquares pieceSquares where
    pieceSquares = squares pieceBoard
    pseudoAttackedSquares square = fmap ($ square) (restrictedMoves side piece square restriction) where
      restriction square = onBoard square && (not $ isFilled square occupiedBoard)
  _    -> undefined

getCaptureBoard :: Position -> Side -> Board
getCaptureBoard position side = let obstacles = getOccupiedSquares position in case side of
  White -> foldl union emptyBoard [
    extendCaptures side Pawn (whitePawns position) obstacles,
    extendCaptures side Knight (whiteKnights position) obstacles,
    extendCaptures side Bishop (whiteBishops position) obstacles,
    extendCaptures side Rook (whiteRooks position) obstacles,
    extendCaptures side Queen (whiteQueens position) obstacles,
    extendCaptures side King (whiteKings position) obstacles
    ]
  Black -> undefined

isLegalMove :: Position -> Move -> Bool
isLegalMove position move = case move of
  -- i.e. O-O from e1 to g1 or e8 to g8.
  CastleShort -> and restrictions where
    restrictions = [
      notAlreadyMovedKingOrRook,
      areIntermediateSquaresEmpty,
      noSquaresAttacked
      ]
    notAlreadyMovedKingOrRook = canSideCastleShort side position
    areIntermediateSquaresEmpty = case side of
      White -> and $ (not . flip isFilled collisions) <$> [f1, g1]
      Black -> and $ (not . flip isFilled collisions) <$> [f8, g8]
    noSquaresAttacked = case side of
      White -> and $ (not . flip isFilled attackedBoard) <$> [e1, f1, g1]
      Black -> and $ (not . flip isFilled attackedBoard) <$> [e8, f8, g8]
    side = sideToMove position
    collisions = getOccupiedSquares position
    attackedBoard = getCaptureBoard position side

  -- i.e. O-O-O from e1 to c1 or e8 to c8.
  CastleLong -> undefined

  PieceCapture origin destination (Just promotionPiece) -> undefined
  PieceCapture origin destination Nothing -> undefined

  PieceMovement origin destination (Just promotionPiece) -> undefined

  PieceMovement origin destination Nothing -> and restrictions where
    restrictions = [
      not (side `inCheck` position) || isKing 
      ]
    side = sideToMove position
    isKing = case side of
      White -> isFilled origin (whiteKings position)
      Black -> isFilled origin (blackKings position)

legalMoves :: Position -> [Move]
legalMoves _ = []

-- Would a sliding movement pass over any filled squares on a board?
-- Not including the origin or destination square.
-- Knights do not slide.
isInterposed :: Piece -> AlgebraicSquare -> MovementPattern -> Board -> Bool
isInterposed piece origin m board = case piece of
  King   -> ortho
  Knight -> False
  Bishop -> diag
  Rook   -> ortho
  Queen  -> if isOrtho origin (m origin) then ortho else diag
  Pawn   -> if isOrtho origin (m origin) then ortho else diag
  where
    ortho = any (`isFilled` board) $ orthogonalIntermediateCoverage origin $ m origin
    diag = any (`isFilled` board) $ diagonalIntermediateCoverage origin $ m origin

