import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

import Chess
import Chess.Board
import Chess.Move
import Chess.Position
import Chess.Rules
import Chess.Theory

testPawnPseudoCaptureCentral = TestLabel name $ TestCase (assert $ expected == actual) where
  name = "white pawn on e4 captures [d5, f5]"
  expected = (fillSquare d5 . fillSquare f5) emptyBoard
  actual = pseudoCaptures White Pawn (fillSquare e4 emptyBoard) emptyBoard

testPawnPseudoCaptureLeftWing = TestLabel name $ TestCase (assert $ expected == actual) where
  name = "white pawn on a5 captures [b6]"
  expected = fillSquare b6 emptyBoard
  actual = pseudoCaptures White Pawn (fillSquare a5 emptyBoard) emptyBoard

testPawnPseudoCaptureRightWing = TestLabel name $ TestCase (assert $ expected == actual) where
  name = "white pawn on h7 captures [g8]"
  expected = fillSquare g8 emptyBoard
  actual = pseudoCaptures White Pawn (fillSquare h7 emptyBoard) emptyBoard

testRookPseudoCaptureUnobstructed = TestLabel name $ TestCase (assert $ expected == actual) where
  name = "white rook on e4 captures [a4, b4, c4, d4, f4, g4, h4, e1, e2, e3, e5, e6, e7, e8]"
  expected = fillSquares [a4, b4, c4, d4, f4, g4, h4, e1, e2, e3, e5, e6, e7, e8] emptyBoard
  actual = pseudoCaptures White Rook (fillSquare e4 emptyBoard) emptyBoard

testRookPseudoCaptureObstructed = TestLabel name $ TestCase (assert $ expected == actual) where
  name = "white rook on e4 captures [c4, d4, f4, g4, e2, e3, e5, e6, e7] when there are white pawns on [b4, h4, e1, e8]"
  expected = fillSquares [c4, d4, f4, g4, e2, e3, e5, e6, e7] emptyBoard
  actual = pseudoCaptures White Rook (fillSquare e4 emptyBoard) (fillSquares [b4, h4, e1, e8] emptyBoard)

testBishopPseudoCaptureUnobstructed = TestLabel name $ TestCase (assert $ expected == actual) where
  name = "white bishop on d4 captures [a1, b2, c3, e5, f6, g7, h8, a7, b6, c5, e3, f2, g1]"
  expected = fillSquares [a1, b2, c3, e5, f6, g7, h8, a7, b6, c5, e3, f2, g1] emptyBoard
  actual = pseudoCaptures White Bishop (fillSquare d4 emptyBoard) emptyBoard

testBishopPseudoCaptureObstructed = TestLabel name $ TestCase (assert $ expected == actual) where
  name = "white bishop on d4 captures [b2, c3, e5, f6, b6, c5, e3, f2] when there are white pawns on [a1, a7, g1, g7]"
  expected = fillSquares [b2, c3, e5, f6, b6, c5, e3, f2] emptyBoard
  actual = pseudoCaptures White Bishop (fillSquare d4 emptyBoard) (fillSquares [a1, a7, g1, g7] emptyBoard)

testPseudoCaptures = TestList [
  TestLabel "Pawns" $ TestList [
    testPawnPseudoCaptureCentral,
    testPawnPseudoCaptureLeftWing,
    testPawnPseudoCaptureRightWing
    ],
  TestLabel "Rooks" $ TestList [
    testRookPseudoCaptureUnobstructed,
    testRookPseudoCaptureObstructed
    ],
  TestLabel "Bishops" $ TestList [
    testBishopPseudoCaptureUnobstructed,
    testBishopPseudoCaptureObstructed
    ]
  ]

testKnightPseudoMoves = TestLabel "Knights" $ TestCase $ assertBool name expr where
  name = "A knight on a1 can move to b3 and c2."
  expr = 2 == length (boundedMoves White Knight a1)

testPseudoMoves = TestList [
  testKnightPseudoMoves
  ]

legalShortCastleTest position positionDescr = TestLabel name $ TestCase (assert $ expected == actual) where
  name = "O-O is legal " ++ positionDescr
  expected = True
  actual = legal position CastleShort

illegalShortCastleTest position positionDescr = TestLabel name $ TestCase (assert $ expected == actual) where
  name = "O-O is illegal " ++ positionDescr
  expected = False
  actual = legal position CastleShort

legalLongCastleTest position positionDescr = TestLabel name $ TestCase (assert $ expected == actual) where
  name = "O-O-O is legal " ++ positionDescr
  expected = True
  actual = legal position CastleLong

illegalLongCastleTest position positionDescr = TestLabel name $ TestCase (assert $ expected == actual) where
  name = "O-O-O is illegal " ++ positionDescr
  expected = False
  actual = legal position CastleLong

testCastling = TestLabel "Castling" $ TestList [
  illegalShortCastleTest initialPosition "in the initial position", 
  illegalLongCastleTest initialPosition "in the initial position",
  legalShortCastleTest positionBerlin "in the Ruy Lopez, 3...Nf6"
  ]

legalMoveTest origin destination position positionDescr = TestLabel name $ TestCase (assert $ expected == actual) where
  name = show origin ++ " to " ++ show destination ++ " is legal " ++ positionDescr
  expected = True
  actual = legal position move
  move = PieceMovement origin destination Nothing

illegalMoveTest origin destination position positionDescr = TestLabel name $ TestCase (assert $ expected == actual) where
  name = show origin ++ " to " ++ show destination ++ " is illegal " ++ positionDescr
  expected = False 
  actual = legal position move
  move = PieceMovement origin destination Nothing

testLegalPieceMovementNoPromotion = TestLabel "Piece Movement (w/o promotion)" $ TestList [
  TestLabel "Pawns" $ TestList [
    legalMoveTest e2 e4 initialPosition "in the initial position",
    legalMoveTest e2 e3 initialPosition "in the initial position",
    illegalMoveTest e7 e5 initialPosition "in the initial position",
    illegalMoveTest e7 e6 initialPosition "in the initial position",
    illegalMoveTest e2 e5 initialPosition "in the initial position",
    illegalMoveTest e2 e2 initialPosition "in the initial position",
    illegalMoveTest e2 e1 initialPosition "in the initial position",
    illegalMoveTest e2 f3 initialPosition "in the initial position",
    illegalMoveTest e2 d3 initialPosition "in the initial position",
    illegalMoveTest e7 e4 initialPosition "in the initial position",
    illegalMoveTest e7 e7 initialPosition "in the initial position",
    illegalMoveTest e7 e8 initialPosition "in the initial position",
    illegalMoveTest e7 f6 initialPosition "in the initial position",
    illegalMoveTest e7 d6 initialPosition "in the initial position"
    ],
  TestLabel "Knights" $ TestList [
    legalMoveTest g1 f3 initialPosition "in the initial position",
    legalMoveTest g1 h3 initialPosition "in the initial position",
    illegalMoveTest g8 f6 initialPosition "in the initial position",
    illegalMoveTest g8 h6 initialPosition "in the initial position",
    illegalMoveTest g1 e2 initialPosition "in the initial position",
    legalMoveTest g1 e2 position1e4e5 "after 1.e4 e5",
    illegalMoveTest g8 e7 initialPosition "in the initial position"
    ],
  TestLabel "Bishops" $ TestList [
    legalMoveTest f1 e2 position1e4e5 "after 1.e4 e5",
    legalMoveTest f1 d3 position1e4e5 "after 1.e4 e5",
    legalMoveTest f1 c4 position1e4e5 "after 1.e4 e5",
    legalMoveTest f1 b5 position1e4e5 "after 1.e4 e5",
    legalMoveTest f1 a6 position1e4e5 "after 1.e4 e5",
    illegalMoveTest f1 f3 position1e4e5 "after 1.e4 e5",
    illegalMoveTest f1 g2 position1e4e5 "after 1.e4 e5",
    illegalMoveTest c1 d2 position1e4e5 "after 1.e4 e5",
    illegalMoveTest f8 e7 position1e4e5 "after 1.e4 e5"
    ]
  ]

testLegalMoves = TestList [
  testCastling,
  testLegalPieceMovementNoPromotion
  ]

testInterposition = TestList $ fmap TestCase [
  assert $ orthogonalIntermediateCoverage e2 e4 == [e3],
  assert $ null $ orthogonalIntermediateCoverage e2 e3,
  assert $ diagonalIntermediateCoverage g2 e4 == [f3],
  assert $ null $ diagonalIntermediateCoverage g2 f3,
  assert $ isInterposed Pawn e2 e4 (fillSquare e3 emptyBoard),
  assert $ isInterposed Bishop g2 e4 (fillSquare f3 emptyBoard),
  assert $ not $ isInterposed Pawn e2 e4 (fillSquare e2 emptyBoard),
  assert $ not $ isInterposed Pawn e2 e3 (fillSquare e2 emptyBoard),
  assert $ not $ isInterposed Bishop g2 f3 (fillSquare g2 emptyBoard),
  assert $ not $ isInterposed Bishop g2 e4 (fillSquare g2 emptyBoard)
  ]

prop_boardUnionIdentity = testProperty
  "The union of any board with an empty board is the board itself."
  (\board -> (emptyBoard `union` board) == board)

-- TODO: Generate rank and file sets.
prop_boardIntersectionRowColSingular = testProperty
  "The intersection of a row and column is a square."
  prop where
    prop rank file = rowBoard `intersection` colBoard == singletonBoard where
      rowBoard = foldl (flip fillSquare) emptyBoard rank
      colBoard = foldl (flip fillSquare) emptyBoard file
      singletonBoard = fillSquare (AlgebraicSquare rank file) emptyBoard

main = defaultMain [
  testGroup "Board Representation" [
    prop_boardUnionIdentity,
    prop_boardIntersectionRowColSingular
    ],
  testGroup "Interposition" $ hUnitTestToTests testInterposition,
  testGroup "Pseudo Captures" $ hUnitTestToTests testPseudoCaptures,
  testGroup "Pseudo Moves" $ hUnitTestToTests testPseudoMoves,
  testGroup "Legal Moves" $ hUnitTestToTests testLegalMoves
  ]
