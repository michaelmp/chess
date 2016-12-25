module Chess where

-- There are two players who take turns moving the pieces.
data Side = White | Black deriving (Show, Eq)

other :: Side -> Side
other side = case side of White -> Black; Black -> White

-- Pieces occupy squares and move around doing violent business.
data Piece = King | Queen | Rook | Bishop | Knight | Pawn deriving (Show, Eq)

