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
ais = [ ("firstLegalMove", NoLookahead (firstLegalMove COMP1130)),
        ("greedy", NoLookahead greedy),
        ("default", WithLookahead minimax)
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

-- | Minimax AI: Uses the Minimax algorithm to look n number of moves ahead
-- using a gametree with the assumption that the opponent plays the best 
-- they can using an identical heuristic.
-- Heuristic: Choose the move with the greatest piece advantage (p1 - p2)
minimax :: GameState -> Int -> Move
minimax state depth = minimaxHelper (legalMoves state) (valueTree state depth) (roseChildren (optimalTree state (valueTree state depth)))
  where
  minimaxHelper :: [Move] -> RoseTree (Int, GameState) -> [(Int, GameState)] -> Move
  minimaxHelper moves tree children = case (moves, children) of
    (x:xs, (int,_):ys) -> case applyMove COMP1130 x state of
      Just _ 
        | int == numPieces -> x
        | otherwise -> minimaxHelper xs tree ys
        where
        numPieces = case turn state of
          Turn Player1 -> 50
          Turn Player2 -> -50
          _ -> error "Game Over"
      _ -> Pass
    (_,_) -> Pass

-- | valueTree function generates a RoseTree of tuples consisting of piece
-- advantage and gamestate with the most optimal path replaced with 
-- the greatest piece advantage.
valueTree :: GameState -> Int -> RoseTree (Int, GameState)
valueTree state depth = valueTreeHelper depth (diffTree state depth)
    where
    valueTreeHelper :: Int -> RoseTree (Int, GameState) -> RoseTree (Int, GameState)
    valueTreeHelper depth' (RoseNode x trees) 
      | depth <= 0 = (RoseNode x trees)
      | otherwise = case turn state of
        Turn Player1 -> (RoseNode (fst (maxChildren (RoseNode x trees)), snd x) (map (valueTreeHelper (depth' - 1)) trees))
        Turn Player2 -> (RoseNode (fst (minChildren (RoseNode x trees)), snd x) (map (valueTreeHelper (depth' - 1)) trees))
        _ -> (RoseNode x trees)

-- | optimalTree replaces all nodes with the greatest piece advantage with 50
-- and the least piece advantage with -50 to avoid overlapping piece advantage
-- values.
optimalTree :: GameState -> RoseTree (Int, GameState) -> RoseTree (Int, GameState)
optimalTree state tree = optimalHelper state tree
  where
  optimalHelper :: GameState -> RoseTree (Int, GameState) -> RoseTree (Int, GameState)
  optimalHelper state' tree' = case tree' of
    RoseNode (int, treestate) subtrees -> case turn state' of
      Turn Player1
        | int == fst (maxChildren tree) -> RoseNode (50, treestate) (map (optimalHelper treestate) subtrees)
        | otherwise -> RoseNode (int, treestate) (map (optimalTree treestate) subtrees)
      Turn Player2
        | int == fst (minChildren tree) -> RoseNode (-50, treestate) (map (optimalHelper treestate) subtrees)
        | otherwise -> RoseNode (int, treestate) (map (optimalTree treestate) subtrees)
      _ -> error "Game Over"

-- | maxChildren function that takes a rosetree node and finds the maximum of 
-- it's children and it's respective gamestate.
maxChildren :: RoseTree (Int, GameState) -> (Int, GameState)
maxChildren tree = maxHelper (roseLeaves tree)
  where
  maxHelper :: [(Int, GameState)] -> (Int, GameState)
  maxHelper list = case list of
    [] -> error "Empty list"
    [(int,state)] -> (int, state)
    (int1, state1):(int2, state2):xs
      | int1 > int2 -> maxHelper ((int1, state1) : xs)
      | otherwise -> maxHelper ((int2, state2) : xs)

-- | minChildren function that takes a rosetree node and finds the minimum of 
-- it's children and it's respective gamestate.
minChildren :: RoseTree (Int, GameState) -> (Int, GameState)
minChildren tree = minHelper (roseLeaves tree)
  where
  minHelper :: [(Int, GameState)] -> (Int, GameState)
  minHelper list = case list of
    [] -> error "Empty list"
    [(int,state)] -> (int, state)
    (int1, state1):(int2, state2):xs
      | int1 < int2 -> minHelper ((int1, state1) : xs)
      | otherwise -> minHelper ((int2, state2) : xs)
  
-- diffTree generates a roseTree of the piece difference of each state
-- alongside it's respective gamestate.
diffTree :: GameState -> Int -> RoseTree (Int, GameState)
diffTree state int
  | int <= 0  = RoseNode ((pieceDifference state), state) []
  | otherwise = RoseNode ((pieceDifference state), state) (diffTreeHelper state (legalMoves state) int)
  where
  diffTreeHelper :: GameState -> [Move] -> Int -> [RoseTree (Int, GameState)]
  diffTreeHelper st moves n = case moves of
    []   -> []
    x:xs -> case applyMove COMP1130 x st of
      Nothing           -> []
      Just appliedstate -> subTree : (diffTreeHelper st xs n)
        where 
        subTree = diffTree appliedstate (n-1)

-- | pieceDifference returns the piece difference between Player 1 (White)
-- and Player 2 (Black).
pieceDifference :: GameState -> Int
pieceDifference state = case (countPieces state) of
  (p1,p2) -> p1 - p2

-- | gameStates returns the list of possible GameStates given a GameState
gameStates :: GameState -> [GameState]
gameStates state = gameStates' state (legalMoves state)
  where
  gameStates' :: GameState -> [Move] -> [GameState]
  gameStates' st moves = case moves of
    []   -> []
    x:xs -> case applyMove COMP1130 x st of
      Nothing           -> []
      Just appliedstate -> appliedstate : gameStates' st xs 

-- | roseChildren returns the children of a rosetree as a list
roseChildren :: RoseTree a -> [a]
roseChildren (RoseNode _ subtrees) = roseChildrenHelper subtrees
  where
  roseChildrenHelper :: [RoseTree a] -> [a]
  roseChildrenHelper trees = case trees of
    [] -> []
    (RoseNode x _) : xs -> x : roseChildrenHelper xs

-- | roseLeaves returns the roseleaves of a rosetree as a list
roseLeaves :: RoseTree a -> [a]
roseLeaves (RoseNode x subtrees)
    | null subtrees = [x]
    | otherwise     = concatMap roseLeaves subtrees

legalMovesPass :: GameState -> [Move]
legalMovesPass state = case captor state of
  None -> []
  Captor _ _ -> Pass : legalMoves state

pruneValueTree :: GameState -> RoseTree (Int, GameState) -> RoseTree (Int, GameState)
pruneValueTree state (RoseNode (int, treestate) subtrees) = case subtrees of
  [] -> (RoseNode (int, state) [])
  x:xs -> undefined

firstAlphaBeta :: GameState -> RoseTree (Int, GameState) -> (Int, Int)
firstAlphaBeta state (RoseNode (n, _) subtrees) = case subtrees of
  []   -> case turn state of
    Turn Player1 -> (n,100)
    Turn Player2 -> (-100, n)
    GameOver (Winner (Player1)) -> (n,100)
    GameOver (Winner (Player2)) -> (-100,n)
    GameOver Draw               -> (-100,100)
  (RoseNode (m, state2) []) : _ -> case turn state2 of
    Turn Player1 -> (m,100)
    Turn Player2 -> (-100, m)
    GameOver (Winner (Player1)) -> (m,100)
    GameOver (Winner (Player2)) -> (-100,m)
    GameOver Draw               -> (-100,100)
  (RoseNode (_, _) (t:_)) : _ -> firstAlphaBeta state t


alphaBetaTree :: RoseTree (Int, GameState) -> RoseTree (Int, Int)
alphaBetaTree (RoseNode (int, state) subtrees) = case subtrees of
  [] -> RoseNode (-100,100) []
  x:xs -> undefined


