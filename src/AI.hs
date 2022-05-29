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
greedy state = greedyHelper (legalMovesPass state)
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
 
-- | Minimax AI: Uses the Minimax algorithm to look n number of moves ahead
-- using a gametree with the assumption that the opponent plays the best
-- they can using an identical heuristic.
-- Heuristic: Choose the move with the greatest piece advantage (p1 - p2)
minimax :: GameState -> Int -> Move
minimax state depth = minimaxHelper (legalMovesPass state) (roseChildren (
  mmTree state (valueTree state depth)))
  where
  minimaxHelper :: [Move] -> [(Int, GameState)] -> Move
  minimaxHelper moves children = case (moves, children) of
    (x:xs, (int,_):ys) -> case applyMove COMP1130 x state of
      Just _
        | int == numPieces -> x
        | otherwise -> minimaxHelper xs ys
        where
        numPieces = case turn state of
          Turn Player1 -> 50
          Turn Player2 -> -50
          _ -> error "Game Over"
      _ -> Pass
    (_,_) -> Pass
 
-- | gameTree generates a rosetree of tuples containing a state and the
-- piece difference in that state.
gameTree :: (a -> [a]) -> a -> RoseTree a
gameTree f a = RoseNode a (map (gameTree f) (f a))
 
-- | cutDepth cuts a tree to n depth.
cutDepth :: Int -> RoseTree a -> RoseTree a
cutDepth int (RoseNode x subtrees)
  | int <= 0 = (RoseNode x [])
  | otherwise = (RoseNode x (map (cutDepth (int - 1)) subtrees))
 
-- | unMaybe takes a Maybe GameState and returns GameState.
unMaybe :: Maybe GameState -> GameState
unMaybe (Just state) = state
unMaybe _            = error "Not a gamestate"
 
-- | gameStates takes a tuple of a game state and piece difference and
-- generates a list of the next possible game states and their respective
-- piece differences.
gameStates :: (Int, GameState) -> [(Int, GameState)]
gameStates (_,state) = map (\x -> (pieceDifference x, x)) (
  map unMaybe [applyMove COMP1130 moves state | moves <- legalMovesPass state])
 
-- | valueTree function generates a RoseTree of tuples consisting of piece
-- advantage and gamestate with the most optimal path replaced with
-- the greatest piece advantage.
valueTree :: GameState -> Int -> RoseTree (Int, GameState)
valueTree state depth = valueTree' depth (
  cutDepth depth (gameTree gameStates (0, state)))
    where
    valueTree' :: Int -> RoseTree (Int, GameState) -> RoseTree (Int, GameState)
    valueTree' depth' (RoseNode x trees)
      | depth <= 0 = (RoseNode x trees)
      | otherwise = case turn state of
        Turn Player1 -> (RoseNode (fst (maxLeaves (
          RoseNode x trees)), snd x) (map (valueTree' (depth' - 1)) trees))
        Turn Player2 -> (RoseNode (fst (minLeaves (
          RoseNode x trees)), snd x) (map (valueTree' (depth' - 1)) trees))
        _ -> (RoseNode x trees)
 
-- | mmTree replaces all nodes with the greatest piece advantage with 50
-- and the least piece advantage with -50 to avoid overlapping piece advantage
-- values.
mmTree :: GameState -> RoseTree (Int, GameState) -> RoseTree (Int, GameState)
mmTree state tree = mmTree' state tree
  where
  mmTree' :: GameState -> RoseTree (Int, GameState) ->RoseTree (Int, GameState)
  mmTree' state' tree' = case tree' of
    RoseNode (int, treestate) subtrees -> case turn state' of
      Turn Player1
        | int == fst (maxLeaves tree) -> RoseNode (50, treestate) (
          map (mmTree' treestate) subtrees)
        | otherwise -> RoseNode (int, treestate) (
          map (mmTree treestate) subtrees)
      Turn Player2
        | int == fst (minLeaves tree) -> RoseNode (-50, treestate) (
          map (mmTree' treestate) subtrees)
        | otherwise -> RoseNode (int, treestate) (
          map (mmTree treestate) subtrees)
      _ -> error "Game Over"
 
-- | maxLeaves function takes a rosetree node and returns the maximum of
-- it's children and it's respective gamestate.
maxLeaves :: RoseTree (Int, GameState) -> (Int, GameState)
maxLeaves tree = maxHelper (roseLeaves tree)
  where
  maxHelper :: [(Int, GameState)] -> (Int, GameState)
  maxHelper list = case list of
    [] -> error "Empty list"
    [(int,state)] -> (int, state)
    (int1, state1):(int2, state2):xs
      | int1 > int2 -> maxHelper ((int1, state1) : xs)
      | otherwise -> maxHelper ((int2, state2) : xs)
 
-- | minLeaves function takes a rosetree node and returns the minimum of
-- it's children and it's respective gamestate.
minLeaves :: RoseTree (Int, GameState) -> (Int, GameState)
minLeaves tree = minHelper (roseLeaves tree)
  where
  minHelper :: [(Int, GameState)] -> (Int, GameState)
  minHelper list = case list of
    [] -> error "Empty list"
    [(int,state)] -> (int, state)
    (int1, state1):(int2, state2):xs
      | int1 < int2 -> minHelper ((int1, state1) : xs)
      | otherwise -> minHelper ((int2, state2) : xs)

-- | pieceDifference returns the piece difference in Player 1 (White)
-- and Player 2 (Black).
pieceDifference :: GameState -> Int
pieceDifference state = case (countPieces state) of
  (p1,p2) -> p1 - p2
 
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
 
-- | legalMovesPass appends a Passing move onto the list of legal moves
-- which can be made by the AI.
legalMovesPass :: GameState -> [Move]
legalMovesPass state = case captor state of
  None -> legalMoves state
  Captor _ _ -> Pass : legalMoves state
 
-- | Attempt on Alpha-Beta Pruning AI
pruneAB :: GameState -> Int -> Move
pruneAB state depth = pruneHelp state depth (roseChildren (
  mmTree state (valueTree state depth)))
  where
  pruneHelp :: GameState -> Int -> [(Int, GameState)] -> Move
  pruneHelp = undefined
 
pruneTree :: RoseTree (Int, GameState) -> RoseTree (Int, GameState)
pruneTree (RoseNode (int, state) list) =
  (RoseNode (int, state) (pruneHelper list))
  where
  pruneHelper :: [RoseTree (Int, GameState)] -> [RoseTree (Int, GameState)]
  pruneHelper trees = case trees of
    [] -> []
    (RoseNode (int',state') subtrees):xs -> case turn state of
      Turn Player1
        | (RoseNode (int',state') subtrees) == maxSubTree trees -> [(
          RoseNode (int',state') (pruneHelper subtrees))]
        | otherwise -> pruneHelper xs
      Turn Player2
        | (RoseNode (int',state') subtrees) == minSubTree trees -> [(
          RoseNode (int',state') (pruneHelper subtrees))]
        | otherwise -> pruneHelper xs
      _ -> error "Game Over"
    
maxSubTree :: [RoseTree (Int, GameState)] -> RoseTree (Int, GameState)
maxSubTree list = case list of
  [] -> error "Empty list"
  [x] -> x
  (RoseNode (int1, state1) lst):(RoseNode (int2, state2) lst'):xs
    | int1 > int2 -> maxSubTree ((RoseNode (int1, state1) lst) : xs)
    | otherwise -> maxSubTree ((RoseNode (int2, state2) lst') : xs)
 
minSubTree :: [RoseTree (Int, GameState)] -> RoseTree (Int, GameState)
minSubTree list = case list of
  [] -> error "Empty list"
  [x] -> x
  (RoseNode (int1, state1) lst):(RoseNode (int2, state2) lst'):xs
    | int1 < int2 -> minSubTree ((RoseNode (int1, state1) lst) : xs)
    | otherwise -> minSubTree ((RoseNode (int2, state2) lst') : xs)
 
roseSize :: RoseTree a -> Int
roseSize (RoseNode _ subtrees) = 1 + sum(map roseSize subtrees)