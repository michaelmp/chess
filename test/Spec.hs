import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import Chess
import Chess.Board
import Chess.Move
import Chess.Position
import Chess.Rules

-- TODO: Use quick check.
testUnionIdentity = TestCase $ assertEqual name unionBoard anyBoard where
    name = "identity: empty U any == any"
    unionBoard = emptyBoard `union` anyBoard
    anyBoard = fillSquare e4 emptyBoard

testBoardUnion = TestList [
  testUnionIdentity
  ]

testPawnPseudoCaptureCentral = TestLabel name $ TestCase (assert $ expected == actual) where
  name = "white pawn on e4 captures [d5, f5]"
  expected = (fillSquare d5 . fillSquare f5) emptyBoard
  actual = extendCaptures White Pawn (fillSquare e4 emptyBoard) emptyBoard

testPawnPseudoCaptureLeftWing = TestLabel name $ TestCase (assert $ expected == actual) where
  name = "white pawn on a5 captures [b6]"
  expected = fillSquare b6 emptyBoard
  actual = extendCaptures White Pawn (fillSquare a5 emptyBoard) emptyBoard

testPawnPseudoCaptureRightWing = TestLabel name $ TestCase (assert $ expected == actual) where
  name = "white pawn on h7 captures [g8]"
  expected = fillSquare g8 emptyBoard
  actual = extendCaptures White Pawn (fillSquare h7 emptyBoard) emptyBoard

testRookPseudoCaptureUnobstructed = TestLabel name $ TestCase (assert $ expected == actual) where
  name = "white rook on e4 captures [a4, b4, c4, d4, f4, g4, h4, e1, e2, e3, e5, e6, e7, e8]"
  expected = fillSquares [a4, b4, c4, d4, f4, g4, h4, e1, e2, e3, e5, e6, e7, e8] emptyBoard
  actual = extendCaptures White Rook (fillSquare e4 emptyBoard) emptyBoard

testRookPseudoCaptureObstructed = TestLabel name $ TestCase (assert $ expected == actual) where
  name = "white rook on e4 captures [c4, d4, f4, g4, e2, e3, e5, e6, e7] when there are white pawns on [b4, h4, e1, e8]"
  expected = fillSquares [c4, d4, f4, g4, e2, e3, e5, e6, e7] emptyBoard
  actual = extendCaptures White Rook (fillSquare e4 emptyBoard) (fillSquares [b4, h4, e1, e8] emptyBoard)

testPseudoCaptures = TestList [
  TestLabel "Pawns" $ TestList [
    testPawnPseudoCaptureCentral,
    testPawnPseudoCaptureLeftWing,
    testPawnPseudoCaptureRightWing
    ],
  TestLabel "Rooks" $ TestList [
    testRookPseudoCaptureUnobstructed,
    testRookPseudoCaptureObstructed
    ]
  ]

-- TODO: Use quick check.
testRowColumnIntersection = TestCase $ assertEqual name intersectionBoard singletonBoard where
  name = "intersection: row X column = 1 square"
  singletonBoard = fillSquare pivot emptyBoard
  intersectionBoard = rowBoard `intersection` columnBoard
  rowBoard = foldl (flip fillSquare) emptyBoard rank4
  columnBoard = foldl (flip fillSquare) emptyBoard eFile
  pivot = e4

testBoardIntersection = TestList [
  testRowColumnIntersection
  ]

testBoardRepresentation = TestList [
  testBoardUnion,
  testBoardIntersection
  ]

testKnightPseudoMoves = TestLabel "Knights" $ TestCase $ assertBool name expr where
  name = "A knight on a1 can move to b3 and c2."
  expr = 2 == length (boundedMoves White Knight a1)

testPseudoMoves = TestList [
  testKnightPseudoMoves
  ]

testCastling = TestLabel "Castling" $ TestList $ TestCase <$> assertions where
  assertions = [
    assert $ not (isLegalMove initialPosition CastleShort),
    assert $ not (isLegalMove initialPosition CastleLong)
    ]

testLegalMoves = TestList [
  testCastling
  ]

tests = TestList [
  TestLabel "Board Representation" testBoardRepresentation,
  TestLabel "Pseudo Captures" testPseudoCaptures,
  TestLabel "Pseudo Moves" testPseudoMoves,
  TestLabel "Legal Moves" testLegalMoves
  ]

main = defaultMain $ hUnitTestToTests tests
