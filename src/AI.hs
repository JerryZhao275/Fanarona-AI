{-|
Module      : AI
Description : AIs for Fanorona
Copyright   : (c) 2022 ANU JERRY ZHAO
License     : AllRightsReserved
-}
module AI where

import           Fanorona

-- | Type of AI functions you can choose to write.
data AIFunc
  = NoLookahead (GameState -> Move)
    -- ^ Simple AIs that do not need lookahead.
  | WithLookahead (GameState -> Int -> Move)
    -- ^ AIs that want to look ahead. The assignment framework will
    -- call the function over and over with increasing integer
    -- arguments @1, 2, 3, ...@ until your AI's time limit is up. 

-- | The table of all AIs that your assignment provides. The AI named
-- "default" in this table is the one your tutor will dedicate most of
-- his or her attention to marking.
ais :: [(String, AIFunc)]
ais = [ ("Alpha", NoLookahead (firstLegalMove COMP1100)),
        ("Beta", NoLookahead greedy),
        ("Gamma", WithLookahead minimax)
      ]

-- | A very simple AI, which passes whenever it can, and if not,
-- picks the first move returned by the 'legalMoves' function.
-- By default, this function is called on COMP1100 rules, and so
-- will never pass.
-- AIs can rely on the 'legalMoves' list being
-- non-empty; if there were no legal moves, the framework would have
-- ended the game.
firstLegalMove :: Course -> GameState -> Move
firstLegalMove course state = case applyMove course Pass state of
  Nothing -> head (legalMoves state)
  _ -> Pass

-- | An AI that will always make the move that will give the greatest
-- increase in heuristic.
-- Heuristic: taking the greatest number of pieces in the current turn
greedy :: GameState -> Move
greedy state = greedyHelper (legalMoves state)
  where
  greedyHelper :: [Move] -> Move
  greedyHelper moves = case moves of
    [x]                       -> x
    x:xs
      | pieces x == minOfList -> x
      | otherwise             -> greedyHelper xs
    _                         -> Pass
    where
    minOfList = minimum (opponentPieces (listOfPieces (legalMoves state))) 
  -- Helper function that finds the number of opponent pieces after a move
  pieces :: Move -> Int
  pieces move = case applyMove COMP1130 move state of
    Just appliedstate -> case (turn state, countPieces appliedstate) of
      (Turn Player1, (_,p2)) -> p2
      (Turn Player2, (p1,_)) -> p1
      _                      -> 0
    Nothing -> 0
  -- Helper function that generates the number of pieces for (Player1, Player2)
  listOfPieces :: [Move] -> [(Int,Int)]
  listOfPieces moves = case moves of
    []   -> []
    x:xs -> case applyMove COMP1130 x state of
      Just appliedstate -> countPieces appliedstate : listOfPieces xs
      Nothing           -> []
  -- Helper function that returns the list of opponent pieces
  opponentPieces :: [(Int,Int)] -> [Int]
  opponentPieces list = case (turn state, list) of
    (Turn Player1, (_,p2):xs) -> p2 : opponentPieces xs
    (Turn Player2, (p1,_):xs) -> p1 : opponentPieces xs
    _                         -> []


-- | GameTree datatype for building a tree consisting of GameStates as vertices
data RoseTree a = RoseNode a [RoseTree a]
  deriving (Eq, Show)

-- | Function that takes a GameState and depth as input and generates the GameTree 
-- of that depth
gameTree :: GameState -> Int -> RoseTree GameState
gameTree state int
  | int <= 0  = RoseNode state []
  | otherwise = RoseNode state (gameTreeHelper state (legalMoves state) int)
  where
  gameTreeHelper :: GameState -> [Move] -> Int -> [RoseTree GameState]
  gameTreeHelper st moves n = case moves of
    []   -> []
    x:xs -> case applyMove COMP1130 x st of
      Nothing           -> []
      Just appliedstate -> subTree : (gameTreeHelper st xs n)
        where 
        subTree = gameTree appliedstate (n-1)

minimax :: GameState -> Int -> Move
minimax state depth = minimax' (gameStates state) 0
  where
  minimax' :: [GameState] -> Int -> Move
  minimax' states acc = case states of
    x:xs
      | pieceDifference x == maximum (map pieceDifference states) -> (legalMoves state) !! acc
      | pieceDifference x == minimum (map pieceDifference states) -> minimax' xs (acc + 1)
      | otherwise -> minimax' xs (acc + 1)
    _ -> Pass

-- | Piece difference between Player 1 (White) and Player 2 (Black)
-- Larger values = wdvantage for white, smaller values = advantage for black
pieceDifference :: GameState -> Int
pieceDifference state = case (countPieces state) of
  (p1,p2) -> p1 - p2

-- | Returns the list of possible GameStates given a GameState
gameStates :: GameState -> [GameState]
gameStates state = gameStates' state (legalMoves state)
  where
  gameStates' :: GameState -> [Move] -> [GameState]
  gameStates' st moves = case moves of
    []   -> []
    x:xs -> case applyMove COMP1130 x st of
      Nothing           -> []
      Just appliedstate -> appliedstate : gameStates' st xs 

{-}
  Turn Player1 -> p1ValueTree (gameTree state depth) state depth
  Turn Player2 -> p2ValueTree (gameTree state depth) state depth
  where
  -- These two functions should have the integers as the best outcome for the respective players
  -- When finished computing, the primary function should pattern match with Int in (RoseTree (Int, GameState))
  -- Set the new GameState as the GameState in (RoseTree (Int, GameState)) and recurse the function
  p1ValueTree :: RoseTree GameState -> GameState -> Int -> RoseTree (Int, GameState)
  p1ValueTree gametree state' depth' = undefined

  p2ValueTree :: RoseTree GameState -> GameState -> Int -> RoseTree (Int, GameState)
  p2ValueTree gametree state' depth' = undefined
-}

roseLeaves :: RoseTree a -> [a]
roseLeaves (RoseNode x subtrees)
  | null subtrees = [x]
  | otherwise     = concatMap roseLeaves subtrees