{-|
Module      : Fanorona
Description : Tests for the Fanorona game
Copyright   : (c) 2022 The Australian National University
License     : AllRightsReserved
-}
module FanoronaTests where

import           Fanorona
import           Data.Aeson
import           Dragons.Fanorona      ()
import           Dragons.Fanorona.Text
import           Testing

fanoronaTests :: Test
fanoronaTests = TestGroup "Fanorona" 
   [ initialStateTests (1,2)
   , initialStateTests (4,2)
   , initialStateTests (6,6) 
   , legalMovesTests
   , countPiecesTests
   , applyMoveTests
--   , jsonTests
--   , moveParsingTests
  ]


initialStateTests :: (Int, Int) -> Test
initialStateTests (a,b) = TestGroup "initialState"
  [ Test "Correct Height"
      (assertEqual (length (board testState)) (2*b+1))
  , Test "Correct Widths"
      (assertEqual (boardWidths testState) (replicate (2*b+1) (2*a+1)))
  , Test "Centre Square is Empty"
      (assertEqual (locationsOf Empty testState) [Location a b])
  , Test "Equal Numbers of Pieces"
       (assertEqual 
       (length . filter (== Piece Player1) . concat . board $ testState)
       (length . filter (== Piece Player2) . concat . board $ testState))
  ]
  where testState = initialState (a,b)

legalMovesTests :: Test
legalMovesTests = TestGroup "legalMoves"
  [ Test "on intialState"
      (assertEqual (legalMoves (initialState (4, 2)))
        -- Approaches
        [ Move Approach (Location 5 3) (Location 4 2)
        , Move Approach (Location 3 2) (Location 4 2)
        , Move Approach (Location 4 3) (Location 4 2)
        , Move Approach (Location 3 3) (Location 4 2) 
        -- Withdrawals
        , Move Withdrawal (Location 3 2) (Location 4 2)
        ])
  ]

countPiecesTests :: Test
countPiecesTests = TestGroup "countPieces"
   [ Test "on small custom board"
       (assertEqual (countPieces (State (Turn Player1) None  (8, 5) testBoard []))
         (11, 7))
   ]

applyMoveTests :: Test
applyMoveTests = TestGroup "applyMove" $ concat
  [[ Test "capturing on small custom board, turn changing"
      (assertEqual
        (applyMove c
          (Move Withdrawal (Location 4 1) (Location 4 0))
          (State (Turn Player2) None  (5, 5) testBoard []))
        (Just (State (Turn Player1)
              None
              (5, 5) testBoard' [])))

  , Test "capture sequence 1"
      (assertEqual
        (applyMove c
          (Move Approach (Location 2 3) (Location 2 2))
          (State (Turn Player1)
          None
          (5, 5) testBoard' []))
        (Just (State (Turn Player1)
              (Captor (Location 2 2) [Location 2 3])
              (5, 5) testBoard'' [])))
  
  , Test "capture sequence 2"
      (assertEqual
        (applyMove c
          (Move Approach (Location 2 2) (Location 1 2))
          (State (Turn Player1)
          (Captor (Location 2 2) [Location 2 3])
          (5, 5) testBoard'' []))
        (Just (State (Turn Player1)
              (Captor (Location 1 2) [Location 2 2,Location 2 3])
              (5, 5) testBoard''' [])))


  ] | c <- [COMP1100, COMP1130]]
  ++ [
    Test "Pass"
      (assertEqual
        (applyMove COMP1130 Pass
        (State (Turn Player1)
              (Captor (Location 1 2) [Location 2 2,Location 2 3])
              (5, 5) testBoard''' []))
        (Just (State (Turn Player2)
              None
              (5, 5) testBoard''' [])))
     ]


jsonTests :: Test
jsonTests = TestGroup "JSON encode/decode"
  [ Test ("simple encode/decode of Move " ++ show mv)
      (assertEqual (decode (encode mv)) (Just mv))
      | mv <- 
        [
        Pass
        , Move Approach (Location 5 6) (Location 4 7)
        , Move Withdrawal (Location 1 2) (Location 2 2)
        , Move Paika (Location 2 1) (Location 1 1)] 
  ]

moveParsingTests :: Test
moveParsingTests = TestGroup "move parsing/unparsing"
  [ Test "reading roundtrip"
      (assertEqual (renderMove <$> parseMove st "A6-A5") (Just "A6-A5"))
  , Test "printing roundtrip"
      (assertEqual
        (parseMove st (renderMove (Move Paika (Location 5 4) (Location 4 5))))
        (Just (Move Paika (Location 5 4) (Location 4 5))))
  ]
  where st = initialState (4, 4)

boardWidths :: GameState -> [Int]
boardWidths = map length . board

-- | A small example board
testBoard :: Board
testBoard = toBoard
  [
-- ABCDE
  "xxx  ", -- 0
  "x x x", -- 1
  "x  oo", -- 2
  "ooo o", -- 3
  "ooooo"  -- 4
  ]

-- | x (Player2) moves E1-E0-W.
testBoard' :: Board
testBoard' = toBoard
  [
-- ABCDE
  "xxx x", -- 0
  "x x  ", -- 1
  "x  o ", -- 2
  "ooo  ", -- 3
  "oooo "  -- 4
  ]

-- | o (Player1) moves C3-C2-A
testBoard'' :: Board
testBoard'' = toBoard
  [
-- ABCDE
  "xx  x", -- 0
  "x    ", -- 1
  "x oo ", -- 2
  "oo   ", -- 3
  "oooo "  -- 4
  ]

-- | o can then move again, C2-B1-A
-- before finally passing. 
testBoard''' :: Board
testBoard''' = toBoard
  [
-- ABCDE
  "xx  x", -- 0
  "x    ", -- 1
  " o o ", -- 2
  "oo   ", -- 3
  "oooo "  -- 4
  ]


toBoard :: [String] -> Board
toBoard = (map . map) parsePiece
  where
   parsePiece c = case c of
    ' ' -> Empty
    'x' -> Piece Player2
    'o' -> Piece Player1
    _   -> error ("parsePiece: " ++ show c ++ "is not a piece.")
