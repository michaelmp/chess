module Chess.Board where

import qualified Data.Char as C
import qualified Data.List as L
import Data.Maybe

import Chess

-- Squares are either light or dark.
data Shade = Light | Dark deriving (Show, Eq)

-- TODO: Algebraic notation is useful for user interaction but a preferable
-- notation for the computer would be strictly numeric. It's up/down/right/left
-- operations would be implemented through bit operations.
--
-- For simplicity, I should extract the algebraic notation from this module
-- wholesale. I should not do this until I am prepared for benchmarking.
class Square a where
  shade :: a -> Shade
  onBoard :: a -> Bool

-- There are 64 squares on a chess board.
--
-- Begin counting from the bottom of White's left-most file,
-- to the top of that file,
-- then from the bottom of the second-from-the-left file,
-- to the top that file.
--
-- In algebraic notation, that's a1 -> a8 -> ...  -> h1 -> h8.
class BottomLeftOrder a where
  toIndex :: a -> Int

rankIndex rank = rank - 1
fileIndex file = C.ord file - C.ord 'a'
bloIndex file rank = fileIndex file * 8 + rankIndex rank

-- Algebraic notation, e.g. 'e2-e4, c7-c5'.
data AlgebraicSquare = AlgebraicSquare {
  file :: Char,
  rank :: Int
  } deriving (Show, Eq)

instance BottomLeftOrder AlgebraicSquare where
  toIndex (AlgebraicSquare file rank) = bloIndex file rank

instance Square AlgebraicSquare where
  shade (AlgebraicSquare file rank)
    | even (rankIndex rank) == even (fileIndex file) = Dark
    | otherwise                                      = Light
  onBoard (AlgebraicSquare file rank) = file `elem` ['a'..'h'] && rank `elem` [1..8]

fromIndex :: Int -> AlgebraicSquare
fromIndex index = AlgebraicSquare file rank where
  file = C.chr $ C.ord 'a' + floor (toRational index / 8) 
  rank = mod index 8 + 1

a1 = AlgebraicSquare 'a' 1
a2 = AlgebraicSquare 'a' 2
a3 = AlgebraicSquare 'a' 3
a4 = AlgebraicSquare 'a' 4
a5 = AlgebraicSquare 'a' 5
a6 = AlgebraicSquare 'a' 6
a7 = AlgebraicSquare 'a' 7
a8 = AlgebraicSquare 'a' 8

b1 = AlgebraicSquare 'b' 1
b2 = AlgebraicSquare 'b' 2
b3 = AlgebraicSquare 'b' 3
b4 = AlgebraicSquare 'b' 4
b5 = AlgebraicSquare 'b' 5
b6 = AlgebraicSquare 'b' 6
b7 = AlgebraicSquare 'b' 7
b8 = AlgebraicSquare 'b' 8

c1 = AlgebraicSquare 'c' 1
c2 = AlgebraicSquare 'c' 2
c3 = AlgebraicSquare 'c' 3
c4 = AlgebraicSquare 'c' 4
c5 = AlgebraicSquare 'c' 5
c6 = AlgebraicSquare 'c' 6
c7 = AlgebraicSquare 'c' 7
c8 = AlgebraicSquare 'c' 8

d1 = AlgebraicSquare 'd' 1
d2 = AlgebraicSquare 'd' 2
d3 = AlgebraicSquare 'd' 3
d4 = AlgebraicSquare 'd' 4
d5 = AlgebraicSquare 'd' 5
d6 = AlgebraicSquare 'd' 6
d7 = AlgebraicSquare 'd' 7
d8 = AlgebraicSquare 'd' 8

e1 = AlgebraicSquare 'e' 1
e2 = AlgebraicSquare 'e' 2
e3 = AlgebraicSquare 'e' 3
e4 = AlgebraicSquare 'e' 4
e5 = AlgebraicSquare 'e' 5
e6 = AlgebraicSquare 'e' 6
e7 = AlgebraicSquare 'e' 7
e8 = AlgebraicSquare 'e' 8

f1 = AlgebraicSquare 'f' 1
f2 = AlgebraicSquare 'f' 2
f3 = AlgebraicSquare 'f' 3
f4 = AlgebraicSquare 'f' 4
f5 = AlgebraicSquare 'f' 5
f6 = AlgebraicSquare 'f' 6
f7 = AlgebraicSquare 'f' 7
f8 = AlgebraicSquare 'f' 8

g1 = AlgebraicSquare 'g' 1
g2 = AlgebraicSquare 'g' 2
g3 = AlgebraicSquare 'g' 3
g4 = AlgebraicSquare 'g' 4
g5 = AlgebraicSquare 'g' 5
g6 = AlgebraicSquare 'g' 6
g7 = AlgebraicSquare 'g' 7
g8 = AlgebraicSquare 'g' 8

h1 = AlgebraicSquare 'h' 1
h2 = AlgebraicSquare 'h' 2
h3 = AlgebraicSquare 'h' 3
h4 = AlgebraicSquare 'h' 4
h5 = AlgebraicSquare 'h' 5
h6 = AlgebraicSquare 'h' 6
h7 = AlgebraicSquare 'h' 7
h8 = AlgebraicSquare 'h' 8

aFile = AlgebraicSquare 'a' <$> [1..8]
bFile = AlgebraicSquare 'b' <$> [1..8]
cFile = AlgebraicSquare 'c' <$> [1..8]
dFile = AlgebraicSquare 'd' <$> [1..8]
eFile = AlgebraicSquare 'e' <$> [1..8]
fFile = AlgebraicSquare 'f' <$> [1..8]
gFile = AlgebraicSquare 'g' <$> [1..8]
hFile = AlgebraicSquare 'h' <$> [1..8]

rank1 = (`AlgebraicSquare` 1) <$> ['a'..'h']
rank2 = (`AlgebraicSquare` 2) <$> ['a'..'h']
rank3 = (`AlgebraicSquare` 3) <$> ['a'..'h']
rank4 = (`AlgebraicSquare` 4) <$> ['a'..'h']
rank5 = (`AlgebraicSquare` 5) <$> ['a'..'h']
rank6 = (`AlgebraicSquare` 6) <$> ['a'..'h']
rank7 = (`AlgebraicSquare` 7) <$> ['a'..'h']
rank8 = (`AlgebraicSquare` 8) <$> ['a'..'h']

-- The 64 squares and some binary fact about them.
-- TODO: Replace with a byte array representation.
type Board = [Bool]

emptyBoard = replicate 64 False
fullBoard = replicate 64 True

squares :: Board -> [AlgebraicSquare]
squares board = indexToSquare <$> filter snd indexedBits where
  indexToSquare (index, filled) = fromIndex index
  indexedBits = zip [0..63] board

setSquare :: Bool -> AlgebraicSquare -> Board -> Board
setSquare input square board = left ++ value : drop 1 right where
  value = input
  (left, right) = splitAt (toIndex square) board

clearSquare :: AlgebraicSquare -> Board -> Board
clearSquare = setSquare False

fillSquare :: AlgebraicSquare -> Board -> Board
fillSquare = setSquare True

fillSquares :: [AlgebraicSquare] -> Board -> Board
fillSquares squares board = foldl (flip fillSquare) board squares

-- Clear the origin and fill the destination on a board.
moveSquare :: AlgebraicSquare -> AlgebraicSquare -> Board -> Board
moveSquare from to = clearSquare from . fillSquare to 

-- What is the binary value at some square on a board?
isFilled :: AlgebraicSquare -> Board -> Bool
isFilled square board = board !! toIndex square

intermediateValues list = if length list > 2
  then drop 1 $ reverse $ drop 1 list
  else list

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


union :: Board -> Board -> Board
a `union` b = uncurry (||) <$> L.zip a b

intersection :: Board -> Board -> Board
a `intersection` b = uncurry (&&) <$> L.zip a b
