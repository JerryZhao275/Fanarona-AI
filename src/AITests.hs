{-|
Module      : AITests
Description : Tests for your AI functions
Copyright   : (c) 2020 Your Name Here
License     : AllRightsReserved
-}
module AITests where

import           AI
import           Fanorona
import           Testing

aiTests :: Test
aiTests = TestGroup "AI"
  [ greedyTest,
    minimaxTest,
    diffTreeTest,
    valueTreeTest,
    mmTreeTest,
    maxLeavesTest,
    minLeavesTest,
    pieceDifferenceTest,
    roseChildrenTest,
    roseLeavesTest,
    legalMovesPassTest
  ]

greedyTest :: Test
greedyTest = Test (("Greedy AI: ") ++ (
  "Output the greediest move with no look ahead")) (
  assertEqual (greedy (initialState (1,1))) (
    Move Approach (Location 2 2) (Location 1 1) :: Move))

minimaxTest :: Test
minimaxTest = Test (("Minimax AI: Output the move that ") ++ (
  "gives the greatest piece advantage with look ahead")) (
  assertEqual (minimax (initialState (4,2)) 4) (
    Move Approach (Location 3 3) (Location 4 2) :: Move))

diffTreeTest :: Test
diffTreeTest = Test (("diffTree generates a tree of n depth ") ++ (
  "consisting of the gamestate and it's respective piece difference")) (
  assertEqual (diffTree (initialState (0,0)) 1) (
    RoseNode (0,State (Turn Player1) None (1,1) [
    [Empty]] []) [] :: (RoseTree (Int, GameState))))

valueTreeTest :: Test
valueTreeTest = Test (("valueTree generates a tree of n depth ") ++ (
  "with the minimax algorithm applied to it's int values.")) (
  assertEqual (valueTree (initialState (0,0)) 1) (
    RoseNode (0,State (Turn Player1) None (
      1,1) [[Empty]] []) [] :: RoseTree (Int, GameState)))

mmTreeTest :: Test
mmTreeTest = Test (("mmTree replaces the maximum nodes in") ++ (
  " the tree with 50 and minimum nodes with -50")) (
  assertEqual (mmTree (initialState (0,0)) (
    valueTree (initialState (0,0)) 1)) (
    RoseNode (50,State (Turn Player1) None (
      1,1) [[Empty]] []) [] :: RoseTree (Int, GameState)))

maxLeavesTest :: Test
maxLeavesTest = Test (("maxChildren returns the maximum int value") ++ (
  " from the children of a given tree (int = difference in pieces)")) (
  assertEqual (maxLeaves (valueTree (initialState (1,1)) 5)) (
    (2,State (Turn Player2) None (3,3) [[Empty,Empty,Empty],[
    Piece Player2,Empty,Piece Player1],[
    Piece Player1,Piece Player1,Empty]] []) :: (Int, GameState)))

minLeavesTest :: Test
minLeavesTest = Test (("minLeaves returns the minimum int value") ++ (
  " from the children of a given tree (int = difference in pieces)")) (
  assertEqual (minLeaves (valueTree (initialState (1,1)) 5)) (
    (-1,State (Turn Player1) None (3,3) [[
    Empty,Piece Player2,Empty],[Empty,Piece Player2,Empty],[
    Empty,Piece Player1,Empty]] []) :: (Int, GameState)))

pieceDifferenceTest :: Test
pieceDifferenceTest = Test (("pieceDifference returns the difference in") ++ (
  " white/black pieces (>0 means white has more, <0 means black has more")) (
  assertEqual (pieceDifference (initialState (4,2))) (0 :: Int))

roseChildrenTest :: Test
roseChildrenTest = Test (("roseChildrenTest returns the children of a") ++ (
  " root given a rosetree")) (
  assertEqual (roseChildren (valueTree (initialState (0,1)) 5)) (
    [(1,State (GameOver (Winner Player1)) None (1,3) [
    [Empty],[Piece Player1],[Empty]] [])] :: [(Int, GameState)]))

roseLeavesTest :: Test
roseLeavesTest = Test (("roseLeavesTest returns the leaves ") ++ (
  "of a rosetree")) (
  assertEqual (roseLeaves (valueTree (initialState (0,1)) 5)) (
    [(1,State (GameOver (Winner Player1)) None (1,3) [
    [Empty],[Piece Player1],[Empty]] [])] :: [(Int, GameState)]))

legalMovesPassTest :: Test
legalMovesPassTest = Test (("legalMovesPassTest adds a passing") ++ (
  " move to the list of legal moves")) (
  assertEqual (legalMovesPass (initialState (1,1))) (
    [Move Approach (Location 2 2) (Location 1 1),Move Approach (
    Location 2 1) (Location 1 1),Move Approach (Location 1 2) (
    Location 1 1),Move Approach (Location 0 2) (Location 1 1)] :: [Move]))

