{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Chess.Board where

import System.Random
import qualified Data.Char as C
import qualified Data.List as L
import Data.Maybe
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Control.Monad

import Chess

-- Squares are either light or dark.
data Shade = Light | Dark deriving (Show, Eq)

-- TODO: Add all methods declared by AlgebraicSquare to make polymorphic.
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

-- A collection of squares.
class Constellation a where
  squares :: a -> [AlgebraicSquare]

rankIndex rank = rank - 1
fileIndex file = C.ord file - C.ord 'a'
bloIndex file rank = fileIndex file * 8 + rankIndex rank

newtype Rank = Rank Int deriving (Eq, Ord, Enum, Random)
newtype File = File Char deriving (Eq, Ord, Enum, Random)

instance Bounded Rank where
  minBound = Rank 1
  maxBound = Rank 8

instance Bounded File where
  minBound = File 'a'
  maxBound = File 'h'

instance Arbitrary Rank where
  arbitrary = arbitraryBoundedRandom

instance Arbitrary File where
  arbitrary = arbitraryBoundedRandom

instance Show Rank where
  show (Rank rank) = show rank

instance Show File where
  show (File file) = [file]

instance Constellation Rank where
  squares (Rank r) = (`AlgebraicSquare` Rank r) . File <$> ['a'..'h'] 

instance Constellation File where
  squares (File f) = AlgebraicSquare (File f) . Rank <$> [1 .. 8]

-- Algebraic notation, e.g. 'e2-e4, c7-c5'.
data AlgebraicSquare = AlgebraicSquare {
  file :: File,
  rank :: Rank 
  } deriving (Eq, Ord)

showAlgebraicSquare :: AlgebraicSquare -> String
showAlgebraicSquare s = show (file s) ++ show (rank s)

instance Show AlgebraicSquare where
  show = showAlgebraicSquare

instance BottomLeftOrder AlgebraicSquare where
  toIndex (AlgebraicSquare (File file) (Rank rank)) = bloIndex file rank

instance Square AlgebraicSquare where
  shade (AlgebraicSquare (File file) (Rank rank))
    | even (rankIndex rank) == even (fileIndex file) = Dark
    | otherwise                                      = Light
  onBoard (AlgebraicSquare (File file) (Rank rank)) = file `elem` ['a'..'h'] && rank `elem` [1..8]

fromIndex :: Int -> AlgebraicSquare
fromIndex index = AlgebraicSquare (File file) (Rank rank) where
  file = C.chr $ C.ord 'a' + floor (toRational index / 8) 
  rank = mod index 8 + 1

a1 = AlgebraicSquare (File 'a') (Rank 1)
a2 = AlgebraicSquare (File 'a') (Rank 2)
a3 = AlgebraicSquare (File 'a') (Rank 3)
a4 = AlgebraicSquare (File 'a') (Rank 4)
a5 = AlgebraicSquare (File 'a') (Rank 5)
a6 = AlgebraicSquare (File 'a') (Rank 6)
a7 = AlgebraicSquare (File 'a') (Rank 7)
a8 = AlgebraicSquare (File 'a') (Rank 8)

b1 = AlgebraicSquare (File 'b') (Rank 1)
b2 = AlgebraicSquare (File 'b') (Rank 2)
b3 = AlgebraicSquare (File 'b') (Rank 3)
b4 = AlgebraicSquare (File 'b') (Rank 4)
b5 = AlgebraicSquare (File 'b') (Rank 5)
b6 = AlgebraicSquare (File 'b') (Rank 6)
b7 = AlgebraicSquare (File 'b') (Rank 7)
b8 = AlgebraicSquare (File 'b') (Rank 8)

c1 = AlgebraicSquare (File 'c') (Rank 1)
c2 = AlgebraicSquare (File 'c') (Rank 2)
c3 = AlgebraicSquare (File 'c') (Rank 3)
c4 = AlgebraicSquare (File 'c') (Rank 4)
c5 = AlgebraicSquare (File 'c') (Rank 5)
c6 = AlgebraicSquare (File 'c') (Rank 6)
c7 = AlgebraicSquare (File 'c') (Rank 7)
c8 = AlgebraicSquare (File 'c') (Rank 8)

d1 = AlgebraicSquare (File 'd') (Rank 1)
d2 = AlgebraicSquare (File 'd') (Rank 2)
d3 = AlgebraicSquare (File 'd') (Rank 3)
d4 = AlgebraicSquare (File 'd') (Rank 4)
d5 = AlgebraicSquare (File 'd') (Rank 5)
d6 = AlgebraicSquare (File 'd') (Rank 6)
d7 = AlgebraicSquare (File 'd') (Rank 7)
d8 = AlgebraicSquare (File 'd') (Rank 8)

e1 = AlgebraicSquare (File 'e') (Rank 1)
e2 = AlgebraicSquare (File 'e') (Rank 2)
e3 = AlgebraicSquare (File 'e') (Rank 3)
e4 = AlgebraicSquare (File 'e') (Rank 4)
e5 = AlgebraicSquare (File 'e') (Rank 5)
e6 = AlgebraicSquare (File 'e') (Rank 6)
e7 = AlgebraicSquare (File 'e') (Rank 7)
e8 = AlgebraicSquare (File 'e') (Rank 8)

f1 = AlgebraicSquare (File 'f') (Rank 1)
f2 = AlgebraicSquare (File 'f') (Rank 2)
f3 = AlgebraicSquare (File 'f') (Rank 3)
f4 = AlgebraicSquare (File 'f') (Rank 4)
f5 = AlgebraicSquare (File 'f') (Rank 5)
f6 = AlgebraicSquare (File 'f') (Rank 6)
f7 = AlgebraicSquare (File 'f') (Rank 7)
f8 = AlgebraicSquare (File 'f') (Rank 8)

g1 = AlgebraicSquare (File 'g') (Rank 1)
g2 = AlgebraicSquare (File 'g') (Rank 2)
g3 = AlgebraicSquare (File 'g') (Rank 3)
g4 = AlgebraicSquare (File 'g') (Rank 4)
g5 = AlgebraicSquare (File 'g') (Rank 5)
g6 = AlgebraicSquare (File 'g') (Rank 6)
g7 = AlgebraicSquare (File 'g') (Rank 7)
g8 = AlgebraicSquare (File 'g') (Rank 8)

h1 = AlgebraicSquare (File 'h') (Rank 1)
h2 = AlgebraicSquare (File 'h') (Rank 2)
h3 = AlgebraicSquare (File 'h') (Rank 3)
h4 = AlgebraicSquare (File 'h') (Rank 4)
h5 = AlgebraicSquare (File 'h') (Rank 5)
h6 = AlgebraicSquare (File 'h') (Rank 6)
h7 = AlgebraicSquare (File 'h') (Rank 7)
h8 = AlgebraicSquare (File 'h') (Rank 8)

-- The 64 squares and some binary fact about them.
-- TODO: Replace with a byte array representation.
newtype Board = Board [Bool] deriving (Show, Eq)

instance Arbitrary Board where
  arbitrary = fmap Board $ take 64 <$> infiniteListOf choice where
    choice = choose (False, True)

emptyBoard = Board $ replicate 64 False
fullBoard = Board $ replicate 64 True

instance Constellation Board where
  squares (Board board) = indexToSquare <$> filter snd indexedBits where
    indexToSquare (index, filled) = fromIndex index
    indexedBits = zip [0..63] board

setSquare :: Bool -> AlgebraicSquare -> Board -> Board
setSquare input square (Board board) = Board $ left ++ value : drop 1 right where
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
isFilled square (Board board) = board !! toIndex square

intermediateValues list = if length list >= 2
  then drop 1 $ reverse $ drop 1 list
  else list

union :: Board -> Board -> Board
(Board a) `union` (Board b) = Board $ uncurry (||) <$> L.zip a b

intersection :: Board -> Board -> Board
(Board a) `intersection` (Board b) = Board $ uncurry (&&) <$> L.zip a b
