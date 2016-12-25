module Chess.Position where

import qualified System.Console.ANSI as ANSI

import Chess
import Chess.Board

-- A position is the complete state of the game.
data Position = Position {
  -- Whose move is it?
  sideToMove :: Side,

  -- Where are the pieces?
  whitePawns :: Board,
  blackPawns :: Board,
  whiteKnights :: Board,
  blackKnights :: Board,
  whiteBishops :: Board,
  blackBishops :: Board,
  whiteRooks :: Board,
  blackRooks :: Board,
  whiteQueens :: Board,
  blackQueens :: Board,
  whiteKings :: Board,
  blackKings :: Board,

  -- Have the King or Rook moved such that castling is never possible?
  canWhiteCastleShort :: Bool,
  canWhiteCastleLong :: Bool,
  canBlackCastleShort :: Bool,
  canBlackCastleLong :: Bool,

  -- Where is en-passant possible?
  enPassant :: Maybe AlgebraicSquare,

  -- How close are we to the 50 move rule?
  reversibleMoves :: Int
  } deriving (Show, Eq)

canSideCastleShort :: Side -> Position -> Bool
canSideCastleShort side position = case side of
  White -> canWhiteCastleShort position
  Black -> canBlackCastleShort position

canSideCastleLong :: Side -> Position -> Bool
canSideCastleLong side position = case side of
  White -> canWhiteCastleLong position
  Black -> canBlackCastleLong position

kingBoard :: Side -> Position -> Board
kingBoard side position = case side of
  White -> whiteKings position
  Black -> blackKings position

blackPieces :: Position -> Board
blackPieces position = reduce layers where
  reduce = foldl union emptyBoard
  layers = [
    blackPawns position,
    blackKnights position,
    blackBishops position,
    blackRooks position,
    blackQueens position,
    blackKings position
    ]

whitePieces :: Position -> Board
whitePieces position = reduce layers where
  reduce = foldl union emptyBoard
  layers = [
    whitePawns position,
    whiteKnights position,
    whiteBishops position,
    whiteRooks position,
    whiteQueens position,
    whiteKings position
    ]

initialPosition = Position {
  sideToMove = White,

  whitePawns = foldl (flip fillSquare) emptyBoard rank2,
  blackPawns = foldl (flip fillSquare) emptyBoard rank7,
  whiteKnights = fillSquare b1 . fillSquare g1 $ emptyBoard,
  blackKnights = fillSquare b8 . fillSquare g8 $ emptyBoard,
  whiteBishops = fillSquare c1 . fillSquare f1 $ emptyBoard,
  blackBishops = fillSquare c8 . fillSquare f8 $ emptyBoard,
  whiteRooks = fillSquare a1 . fillSquare h1 $ emptyBoard,
  blackRooks = fillSquare a8 . fillSquare h8 $ emptyBoard,
  whiteQueens = fillSquare d1 emptyBoard,
  blackQueens = fillSquare d8 emptyBoard,
  whiteKings = fillSquare e1 emptyBoard,
  blackKings = fillSquare e8 emptyBoard,

  canWhiteCastleShort = True,
  canWhiteCastleLong = True,
  canBlackCastleShort = True,
  canBlackCastleLong = True,

  enPassant = Nothing,

  reversibleMoves = 0
}

getOccupiedSquares :: Position -> Board
getOccupiedSquares position = foldl union emptyBoard [
  whitePawns position, blackPawns position,
  whiteKnights position, blackKnights position,
  whiteBishops position, blackBishops position,
  whiteRooks position, blackRooks position,
  whiteQueens position, blackQueens position,
  whiteKings position, blackKings position
  ]

getPiece :: Position -> AlgebraicSquare -> Maybe (Side, Piece)
getPiece position square
  | isFilled square $ whitePawns position = Just (White, Pawn)
  | isFilled square $ whiteKnights position = Just (White, Knight)
  | isFilled square $ whiteBishops position = Just (White, Bishop)
  | isFilled square $ whiteRooks position = Just (White, Rook)
  | isFilled square $ whiteQueens position = Just (White, Queen)
  | isFilled square $ whiteKings position = Just (White, King)
  | isFilled square $ blackPawns position = Just (Black, Pawn)
  | isFilled square $ blackKnights position = Just (Black, Knight)
  | isFilled square $ blackBishops position = Just (Black, Bishop)
  | isFilled square $ blackRooks position = Just (Black, Rook)
  | isFilled square $ blackQueens position = Just (Black, Queen)
  | isFilled square $ blackKings position = Just (Black, King)
  | otherwise = Nothing

getPieces :: Position -> [(Side, Piece, AlgebraicSquare)]
getPieces position = concat [
  fmap (box White Pawn)   (squares $ whitePawns position),
  fmap (box White Knight) (squares $ whiteKnights position),
  fmap (box White Bishop) (squares $ whiteBishops position),
  fmap (box White Rook)   (squares $ whiteRooks position),
  fmap (box White Queen)  (squares $ whiteQueens position),
  fmap (box White King)   (squares $ whiteKings position),
  fmap (box Black Pawn)   (squares $ blackPawns position),
  fmap (box Black Knight) (squares $ blackKnights position),
  fmap (box Black Bishop) (squares $ blackBishops position),
  fmap (box Black Rook)   (squares $ blackRooks position),
  fmap (box Black Queen)  (squares $ blackQueens position),
  fmap (box Black King)   (squares $ blackKings position)
  ] where
    box side piece square = (side, piece, square)

prettyPrint :: Position -> IO ()
prettyPrint position = do
  putStrLn "+--------+"

  putStr border >> mapM_ printSquare rank8 >> putStrLn border
  putStr border >> mapM_ printSquare rank7 >> putStrLn border
  putStr border >> mapM_ printSquare rank6 >> putStrLn border
  putStr border >> mapM_ printSquare rank5 >> putStrLn border
  putStr border >> mapM_ printSquare rank4 >> putStrLn border
  putStr border >> mapM_ printSquare rank3 >> putStrLn border
  putStr border >> mapM_ printSquare rank2 >> putStrLn border
  putStr border >> mapM_ printSquare rank1 >> putStrLn border

  putStrLn "+--------+"

  where
    border = "|"
    occupier = getPiece position 
    tokenize piece = case piece of
      Just (White, Pawn)   -> 'P'
      Just (Black, Pawn)   -> 'p'
      Just (White, Knight) -> 'N'
      Just (Black, Knight) -> 'n'
      Just (White, Bishop) -> 'B'
      Just (Black, Bishop) -> 'b'
      Just (White, Rook)   -> 'R'
      Just (Black, Rook)   -> 'r'
      Just (White, Queen)  -> 'Q'
      Just (Black, Queen)  -> 'q'
      Just (White, King)   -> 'K'
      Just (Black, King)   -> 'k'
      Nothing              -> ' '
    printSquare square = do
      ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Black]
      ANSI.setSGR [ANSI.SetColor ANSI.Background ANSI.Dull bgColor]
      putChar $ tokenize piece
      ANSI.setSGR [ANSI.Reset]
      where
        piece = occupier square
        bgColor = case shade square of
          Light -> ANSI.White
          Dark  -> ANSI.Cyan

