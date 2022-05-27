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
   greedyTest2,
   minimaxTest,
   minimaxTest2,
   minimaxTest3,
   cutDepthTest,
   unMaybeTest,
   gameStatesTest,
   baseGameStateTest,
   baseValueTreeTest,
   valueTreeTest,
   mmTreeTest,
   baseMaxLeavesTest,
   maxLeavesTest,
   minLeavesTest,
   pieceDifferenceTest,
   baseRoseChildrenTest,
   roseChildrenTest,
   roseLeavesTest,
   legalMovesPassTest
 ]
 
greedyTest :: Test
greedyTest = Test ("Greedy AI base test on initialState (1,1)") (
 assertEqual (greedy (initialState (1,1))) (
   Move Approach (Location 2 2) (Location 1 1) :: Move))
 
greedyTest2 :: Test
greedyTest2 = Test ("Greedy AI on a 9x5 Fanorona board") (
 assertEqual (greedy (initialState (4,2))) (
   Move Approach (Location 5 3) (Location 4 2) :: Move))
 
minimaxTest :: Test
minimaxTest = Test (("Minimax AI base test on initialState (1,1)") ++ (
 "with 1 step to look ahead")) (
 assertEqual (minimax (initialState (1,1)) 1) (
   Move Approach (Location 2 2) (Location 1 1) :: Move))
 
minimaxTest2 :: Test
minimaxTest2 = Test ("Minimax AI on a 9x5 board with 1 look ahead") (
 assertEqual (minimax (initialState (4,2)) 1) (
   Move Approach (Location 5 3) (Location 4 2) :: Move))
 
minimaxTest3 :: Test
minimaxTest3 = Test ("Minimax AI on a 9x5 board with 3 looks ahead") (
 assertEqual (minimax (initialState (4,2)) 3) (
   Move Approach (Location 3 3) (Location 4 2) :: Move))
 
-- | Unable to unit test on gameTree as it produces an infinite tree
-- In this case, cutDepthTest counts as a test for gameTree
cutDepthTest :: Test
cutDepthTest = Test ("cutDepth cuts a tree to n depth.") (
 assertEqual (cutDepth 0 (gameTree gameStates (0, initialState (1,1)))) (
   RoseNode (0,State (Turn Player1) None (3,3) [[Piece Player2,Piece Player2,
   Piece Player2],[Piece Player2,Empty,Piece Player1],[Piece Player1,
   Piece Player1,Piece Player1]] []) [] :: RoseTree (Int, GameState)))
 
unMaybeTest :: Test
unMaybeTest = Test (("unMaybe takes a Maybe GameState and") ++ (
 " returns a GameState")) (
 assertEqual (unMaybe (Just (initialState (1,1)))) (
   State (Turn Player1) None (3,3) [[
   Piece Player2,Piece Player2,Piece Player2],[
   Piece Player2,Empty,Piece Player1],[
   Piece Player1,Piece Player1,Piece Player1]] [] :: GameState))
 
baseGameStateTest :: Test
baseGameStateTest = Test ("Test for the base case of gameStates") (
 assertEqual (gameStates (0,initialState (0,0))) (([]) :: [(Int,GameState)]))
 
gameStatesTest :: Test
gameStatesTest = Test (("gameStates takes a tuple of a game state and ") ++ (
 "piece difference and generates a list of the next possible game") ++ (
 " states and their respective piece differences.")) (
 assertEqual (gameStates (0,initialState (1,1))) ((
   [(1,State (Turn Player2) None (3,3) [[Empty,Piece Player2,Piece Player2],
   [Piece Player2,Piece Player1,Piece Player1],[Piece Player1,Piece Player1,
   Empty]] []),(1,State (Turn Player2) None (3,3) [[Piece Player2,
   Piece Player2,Piece Player2],[Empty,Piece Player1,Empty],[Piece Player1,
   Piece Player1,Piece Player1]] []),(1,State (Turn Player2) None (3,3) [[
   Piece Player2,Empty,Piece Player2],[Piece Player2,Piece Player1,
   Piece Player1],[Piece Player1,Empty,Piece Player1]] []),(1,State (
   Turn Player2) None (3,3) [[Piece Player2,Piece Player2,Empty],[
   Piece Player2,Piece Player1,Piece Player1],[Empty,Piece Player1,
   Piece Player1]] [])]) :: [(Int,GameState)]))
 
baseValueTreeTest :: Test
baseValueTreeTest = Test (("valueTree test on initialState (0,0)") ++ (
 " with a depth of 1")) (
 assertEqual (valueTree (initialState (0,0)) 1) (
   RoseNode (0,State (Turn Player1) None (
     1,1) [[Empty]] []) [] :: RoseTree (Int, GameState)))
 
valueTreeTest :: Test
valueTreeTest = Test (("valueTree test ono initialState (4,2) ") ++ (
 "with a depth of 1")) (
 assertEqual (valueTree (initialState (4,2)) 0) (
   RoseNode (0,State (Turn Player1) None (9,5) [[
   Piece Player2,Piece Player2,Piece Player2,Piece Player2,
   Piece Player2,Piece Player2,Piece Player2,Piece Player2,
   Piece Player2],[Piece Player2,Piece Player2,Piece Player2,
   Piece Player2,Piece Player2,Piece Player2,Piece Player2,
   Piece Player2,Piece Player2],[Piece Player2,Piece Player1,
   Piece Player2,Piece Player1,Empty,Piece Player2,Piece Player1,
   Piece Player2,Piece Player1],[Piece Player1,Piece Player1,
   Piece Player1,Piece Player1,Piece Player1,Piece Player1,
   Piece Player1,Piece Player1,Piece Player1],[Piece Player1,
   Piece Player1,Piece Player1,Piece Player1,Piece Player1,
   Piece Player1,Piece Player1,Piece Player1,Piece Player1]] []) [
   ]:: RoseTree (Int, GameState)))
 
mmTreeTest :: Test
mmTreeTest = Test (("mmTree replaces the maximum nodes in") ++ (
 " the tree with 50 and minimum nodes with -50")) (
 assertEqual (mmTree (initialState (0,0)) (valueTree (
 initialState (0,0)) 1)) (RoseNode (50,State (Turn Player1) None (
 1,1) [[Empty]] []) [] :: RoseTree (Int, GameState)))
 
baseMaxLeavesTest :: Test
baseMaxLeavesTest = Test ("maxChildren test with depth of 1") (
 assertEqual (maxLeaves (valueTree (initialState (1,1)) 1)) (
 (1,State (Turn Player2) None (3,3) [[Piece Player2,Piece Player2,Empty],
 [Piece Player2,Piece Player1,Piece Player1],[Empty,Piece Player1,
 Piece Player1]] []) :: (Int, GameState)))
 
maxLeavesTest :: Test
maxLeavesTest = Test (("maxChildren returns the maximum int value") ++ (
 " from the children of a given tree (int = difference in pieces)")) (
 assertEqual (maxLeaves (valueTree (initialState (1,1)) 5)) ((2,
 State (Turn Player2) None (3,3) [[Empty,Empty,Empty],[Piece Player2,Empty,
 Piece Player1],[Piece Player1,Piece Player1,Empty]] []) :: (Int, GameState)))
 
baseMinLeavesTest :: Test
baseMinLeavesTest = Test ("minLeaves test with depth of 1") (
 assertEqual (minLeaves (valueTree (initialState (1,1)) 1)) (
   (-1,State (Turn Player1) None (3,3) [[
   (1,State (Turn Player2) None (3,3) [[Piece Player2,Piece Player2,Empty],
   [Piece Player2,Piece Player1,Piece Player1],[Empty,Piece Player1,
   Piece Player1]] []) :: (Int, GameState)))
 
minLeavesTest :: Test
minLeavesTest = Test (("minLeaves returns the minimum int value") ++ (
 " from the children of a given tree (int = difference in pieces)")) (
 assertEqual (minLeaves (valueTree (initialState (1,1)) 5)) ((-1,
 State (Turn Player1) None (3,3) [[Empty,Piece Player2,Empty],[Empty,
 Piece Player2,Empty],[Empty,Piece Player1,Empty]] []) :: (Int, GameState)))
 
pieceDifferenceTest :: Test
pieceDifferenceTest = Test (("pieceDifference returns the difference in") ++ (
 " white/black pieces (>0 means white has more, <0 means black has more")) (
 assertEqual (pieceDifference (initialState (4,2))) (0 :: Int))
 
baseRoseChildrenTest :: Test
baseRoseChildrenTest = Test ("roseChildrenTest with a depth of 1") (
 assertEqual (roseChildren (valueTree (initialState (0,1)) 0)) (
 [] :: [(Int, GameState)]))
 
roseChildrenTest :: Test
roseChildrenTest = Test (("roseChildrenTest returns the children of a") ++ (
 " root given a rosetree")) (
 assertEqual (roseChildren (valueTree (initialState (0,1)) 5)) (
 [(1,State (GameOver (Winner Player1)) None (1,3) [[Empty],
 [Piece Player1],[Empty]] [])] :: [(Int, GameState)]))
 
baseRoseLeavesTest :: Test
baseRoseLeavesTest = Test ("roseLeavesTest with a depth of 0") (
 assertEqual (roseLeaves (valueTree (initialState (0,1)) 0)) (
 [(0,State (Turn Player1) None (1,3) [[Piece Player2],[Empty],
 [Piece Player1]] [])] :: [(Int, GameState)]))
 
roseLeavesTest :: Test
roseLeavesTest = Test (("roseLeavesTest returns the leaves ") ++ (
 "of a rosetree")) (
 assertEqual (roseLeaves (valueTree (initialState (0,1)) 5)) (
 [(1,State (GameOver (Winner Player1)) None (1,3) [[Empty],
 [Piece Player1],[Empty]] [])] :: [(Int, GameState)]))
 
legalMovesPassTest :: Test
legalMovesPassTest = Test (("legalMovesPassTest adds a passing") ++ (
 " move to the list of legal moves")) (
 assertEqual (legalMovesPass (initialState (1,1))) ([Move Approach (
 Location 2 2) (Location 1 1),Move Approach (Location 2 1) (Location 1 1),
 Move Approach (Location 1 2) (Location 1 1),
 Move Approach (Location 0 2) (Location 1 1)] :: [Move]))
