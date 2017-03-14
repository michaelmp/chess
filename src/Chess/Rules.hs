module Chess.Rules where

import qualified Data.Char as C

import Chess
import Chess.Board
import Chess.Move
import Chess.Position

inCheck :: Side -> Position -> Bool
side `inCheck` position = and $ kingBoard side position `intersection` getCaptureBoard position (other side)

-- Apply a piece capturing pattern to a board, accounting for occupied squares.
-- TODO: Account for obstructing capturable pieces except for the first in line.
pseudoCaptures :: Side -> Piece -> Board -> Board -> Board
pseudoCaptures side piece pieceBoard occupiedBoard = case piece of
  Pawn   -> (`fillSquares` emptyBoard) $ filter onBoard $ concatMap pseudoAttackedSquares pieceSquares where
    pieceSquares = squares pieceBoard
    pseudoAttackedSquares square = fmap ($ square) [pawnCaptureRight side, pawnCaptureLeft side]
  Knight -> (`fillSquares` emptyBoard) $ concatMap pseudoAttackedSquares pieceSquares where
    pieceSquares = squares pieceBoard
    pseudoAttackedSquares square = fmap ($ square) (restrictedMoves side piece square restriction) where
      restriction = onBoard 
  _      -> (`fillSquares` emptyBoard) $ concatMap pseudoAttackedSquares pieceSquares where
    pieceSquares = squares pieceBoard
    pseudoAttackedSquares square = fmap ($ square) (restrictedMoves side piece square restriction) where
      restriction square = onBoard square && not (isFilled square occupiedBoard)

getCaptureBoard :: Position -> Side -> Board
getCaptureBoard position side = let obstacles = getOccupiedSquares position in case side of
  White -> foldl union emptyBoard [
    pseudoCaptures side Pawn (whitePawns position) obstacles,
    pseudoCaptures side Knight (whiteKnights position) obstacles,
    pseudoCaptures side Bishop (whiteBishops position) obstacles,
    pseudoCaptures side Rook (whiteRooks position) obstacles,
    pseudoCaptures side Queen (whiteQueens position) obstacles,
    pseudoCaptures side King (whiteKings position) obstacles
    ]
  Black -> foldl union emptyBoard [
    pseudoCaptures side Pawn (blackPawns position) obstacles,
    pseudoCaptures side Knight (blackKnights position) obstacles,
    pseudoCaptures side Bishop (blackBishops position) obstacles,
    pseudoCaptures side Rook (blackRooks position) obstacles,
    pseudoCaptures side Queen (blackQueens position) obstacles,
    pseudoCaptures side King (blackKings position) obstacles
    ]

legal :: Position -> Move -> Bool
legal position move = case move of
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

  -- A non-promoting piece movement is legal if all of the following criteria are met:
  -- 1) The movement is not obstructed.
  -- 2) The movement would not result in check on the same side king.
  -- 3) The movement from origin to destination is appropriate for type of piece.
  PieceMovement origin destination Nothing -> and restrictions where
    restrictions = case sideAndPiece of
      Just (side, piece) -> [
        not $ isInterposed piece origin destination (getOccupiedSquares position),
        not $ inCheck side (apply move position),
        any appropriatePattern patterns
        ] where
          appropriatePattern p = p origin == destination
          patterns = boundedMoves side piece origin
      Nothing -> [False]
    sideAndPiece = getPiece position origin
    side = sideToMove position
    isKing = case side of
      White -> isFilled origin (whiteKings position)
      Black -> isFilled origin (blackKings position)

apply :: Move -> Position -> Position
apply move position = case move of
  CastleShort -> undefined

  CastleLong -> undefined

  PieceCapture origin destination (Just promotion) -> undefined
  PieceCapture origin destination Nothing -> undefined

  PieceMovement origin destination (Just promotion)-> undefined

  PieceMovement origin destination Nothing -> Position {
    sideToMove = other $ sideToMove position,
    whitePawns   = moveSquare origin destination (whitePawns position),
    blackPawns   = moveSquare origin destination (blackPawns position),
    whiteKnights = moveSquare origin destination (whiteKnights position),
    blackKnights = moveSquare origin destination (blackKnights position),
    whiteBishops = moveSquare origin destination (whiteBishops position),
    blackBishops = moveSquare origin destination (blackBishops position),
    whiteRooks   = moveSquare origin destination (whiteRooks position),
    blackRooks   = moveSquare origin destination (blackRooks position),
    whiteQueens  = moveSquare origin destination (whiteQueens position),
    blackQueens  = moveSquare origin destination (blackQueens position),
    whiteKings   = moveSquare origin destination (whiteKings position),
    blackKings   = moveSquare origin destination (blackKings position),
    canWhiteCastleShort = canWhiteCastleShort position,
    canWhiteCastleLong = canWhiteCastleLong position,
    canBlackCastleShort = canBlackCastleShort position,
    canBlackCastleLong = canBlackCastleLong position,
    enPassant = enPassant',
    reversibleMoves = if isPawnMove then 0 else reversibleMoves position + 1
    } where
      sideAndPiece = getPiece position origin
      enPassant' = case sideAndPiece of
        Just (side, piece) -> case piece of
          Pawn -> if pawnDoubleAdvance side origin == destination
                       then Just $ pawnAdvance side origin
                       else Nothing
          _    -> Nothing
        Nothing -> Nothing
      isPawnMove = case sideAndPiece of
        Just (side, piece) -> case piece of
          Pawn -> True
          _    -> False
        Nothing -> False

legalMoves :: Position -> [Move]
legalMoves _ = undefined

-- All of the intermediate squares covered (non-inclusive) by an orthogonal movement.
orthogonalIntermediateCoverage :: AlgebraicSquare -> AlgebraicSquare -> [AlgebraicSquare]
orthogonalIntermediateCoverage origin destination = [AlgebraicSquare f r | f <- files, r <- ranks] where
  files = intermediateValues [lesser..greater] where
    lesser = min (file origin) (file destination)
    greater = max (file origin) (file destination)
  ranks = intermediateValues [lesser..greater] where
    lesser = min (rank origin) (rank destination)
    greater = max (rank origin) (rank destination)

-- All of the intermediate squares covered (non-inclusive) by a diagonal
-- movement.
diagonalIntermediateCoverage :: AlgebraicSquare -> AlgebraicSquare -> [AlgebraicSquare]
diagonalIntermediateCoverage origin destination = [AlgebraicSquare (C.chr (C.ord originalFile + fileDiff)) (originalRank + rankDiff) | (fileDiff, rankDiff) <- diffs] where
  originalFile = file origin
  originalRank = rank origin
  diffs = intermediateValues $ (\n -> (n * fileSign, n * rankSign)) <$> take (k + 1) [0..] where
    fileSign = if file destination > file origin then 1 else (-1)
    rankSign = if rank destination > rank origin  then 1 else (-1)
    k = abs $ rank destination - rank origin

-- Would a sliding movement pass over any filled squares on a board?
-- Not including the origin or destination square.
-- Knights do not slide.
isInterposed :: Piece -> Origin -> Destination -> Board -> Bool
isInterposed piece origin destination board = case piece of
  King   -> ortho
  Knight -> False
  Bishop -> diag
  Rook   -> ortho
  Queen  -> if isOrtho origin destination then ortho else diag
  Pawn   -> if isOrtho origin destination then ortho else diag
  where
    ortho = any (`isFilled` board) coverage where
      coverage = orthogonalIntermediateCoverage origin destination
    diag = any (`isFilled` board) coverage where
      coverage = diagonalIntermediateCoverage origin destination

