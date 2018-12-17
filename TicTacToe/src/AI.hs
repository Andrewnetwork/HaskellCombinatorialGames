-- AI.hs
-- Andrew Ribeiro
-- December 2018
-- Tree searching algorithms and heuristics.

module AI where
import TicTacToe
import Data.Maybe
import System.Random
import Data.List
import Data.Ord (comparing)

data GameTree = Terminal EndState | Node Board [GameTree] deriving Show
data Game = Game [Board] EndState
type MovementPolicy = Player -> GameTree -> Move
type Heuristic = Player -> Board -> Int

-- Create a game tree given a current player and board state.
gameTree :: Player -> Board -> GameTree
gameTree player boardState
    | isTerminalState boardState = Node boardState [Terminal (fromJust $ (terminalState boardState))]
    | otherwise = Node boardState children
    where children = map (gameTree (otherPlayer player)) boards
          boards = makeAllMoves player boardState

findTerminalBoards :: Board -> EndState -> GameTree -> [Board]
findTerminalBoards parentNode endState (Terminal x)
    | x == endState = [parentNode]
    | otherwise = []
findTerminalBoards parentNode endState (Node childParent children) = concatMap (findTerminalBoards childParent endState) children

countChildrenEndStates :: EndState -> GameTree -> [(Int,GameTree)]
countChildrenEndStates endState (Node parent children) = zip (map (countChildEndStates endState) children) children

countChildEndStates :: EndState -> GameTree -> Int
countChildEndStates endState (Terminal x)
    | x == endState = 1
    | otherwise = 0
countChildEndStates endState (Node parent children) =  sum (map (countChildEndStates endState) children)
-- countChildEndStates (W O) (gameTree O initialState)

possibilityMatrix :: GameTree -> [[(Int,GameTree)]]
possibilityMatrix tree = map (\x -> x tree) childCounters
                         where childCounters = map countChildrenEndStates [(W X),D,(W O)]

-- Given a game tree, produce a list of all end states.
gameTreeOutcomes :: GameTree -> [EndState]
gameTreeOutcomes (Terminal x) = [x]
gameTreeOutcomes (Node parent children) = concatMap gameTreeOutcomes children

showPossibleEnds :: Board -> EndState -> Player -> [Board]
showPossibleEnds board endState currentPlayer = findTerminalBoards board endState (gameTree currentPlayer board)

makeAllMoves :: Player -> Board -> [Board]
makeAllMoves player boardState = (map (move player boardState) (validMoves player boardState))

gameOutcomes :: Player -> Board -> [EndState]
gameOutcomes player [] = []
gameOutcomes player boardState
    | isTerminalState boardState = maybeToList $ terminalState boardState
    | otherwise = concatMap (gameOutcomes (otherPlayer player)) boards
    where boards = makeAllMoves player boardState

-- ########### Move Policies ######################

pickSelfishMove :: MovementPolicy
pickSelfishMove player (Node parent children) = head$boardPositionDiff parent (getParent$snd$last$sortedCounts)
                                                where countTups = countChildrenEndStates (W player) (Node parent children)
                                                      sortedCounts = (sortBy (\(a,_)(b,_)->compare a b) countTups)

pickAltruisticMove :: MovementPolicy
pickAltruisticMove player (Node parent children) = head$boardPositionDiff parent (getParent$snd$last$sortedCounts)
                                                   where countTups = countChildrenEndStates (W (otherPlayer player)) (Node parent children)
                                                         sortedCounts = (sortBy (\(a,_)(b,_)->compare a b) countTups)

pickSpitefulMove :: MovementPolicy
pickSpitefulMove player (Node parent children) = head$boardPositionDiff parent (getParent$snd$head$sortedCounts)
                                                 where countTups = countChildrenEndStates (W (otherPlayer player)) (Node parent children)
                                                       sortedCounts = (sortBy (\(a,_)(b,_)->compare a b) countTups)

minimaxMove :: MovementPolicy
minimaxMove player tree@(Node parent children) = head $ boardPositionDiff parent bestBoard
                                                 where childBoards = map getParent children
                                                       miniMaxChildren = zip (minimax X tree) childBoards
                                                       (_,bestBoard) = maximumBy (comparing fst) miniMaxChildren

heuristicMove :: Heuristic -> MovementPolicy
heuristicMove heuristic player tree@(Node parent children) = head $ boardPositionDiff bestBoard parent
                                                        where (_,bestBoard) = maximumBy (comparing fst) (boardHeuristics player heuristic tree)
-- #####################################

-- ########### Move Heurisics ######################
childishHeuristic :: Heuristic
childishHeuristic player board = length $ filter (\row-> elem (P player) row) winPaths
                                 where winPaths = getThrees board

selfishHeuristic :: Heuristic
selfishHeuristic player board = sum $ map (\path -> winBuff $ length $ filter ((P player)==) path) nonObsWinPaths
                                where winPaths = getThrees board
                                      nonObsWinPaths = filter (\row-> (elem (P player) row) && not (elem (P (otherPlayer player)) row)) winPaths

blendedSelfishHeuristic :: Heuristic
blendedSelfishHeuristic player board = (selfishHeuristic player board) - (selfishHeuristic (otherPlayer player) board)

winBuff :: (Ord p, Num p) => p -> p
winBuff i
  | i >= 3 = 1000
  | otherwise = i

boardHeuristics :: Player -> Heuristic -> GameTree -> [(Int,Board)]
boardHeuristics player heuristic (Node _ children) = zip heuristicValues possibleBoards
                                                          where possibleBoards = map getParent children
                                                                heuristicValues = map (heuristic player) possibleBoards

-- boardHeuristics O cSelfishHeuristic (gameTree O [[(P X),(P X),E],[E,(P O),E],[E,E,E]])
-- boardHeuristics X cSelfishHeuristic (gameTree O [[(P X),(P X),E],[E,(P O),E],[E,E,E]])

-- cSelfishHeuristic X [[(P X),E,(P X)],[E,(P X),E],[(P O),E,(P O)]]
-- cSelfishHeuristic O [[(P X),E,(P X)],[E,(P X),E],[(P O),E,(P O)]]
--
-- cSelfishHeuristic O [[(P X),E,(P X)],[E,(P X),E],[(P O),(P O),(P O)]]
-- cSelfishHeuristic O [[(P X),(P O),(P X)],[E,(P X),E],[(P O),E,(P O)]]
-- cSelfishHeuristic X [[(P X),E,(P X)],[E,(P X),E],[(P O),(P O),(P O)]]
-- cSelfishHeuristic X [[(P X),(P O),(P X)],[E,(P X),E],[(P O),E,(P O)]]
-- #####################################



getParent :: GameTree -> Board
getParent (Node parent _) = parent

allEndStateGlobalCount :: Player -> Board -> [[Int]]
allEndStateGlobalCount player board = map (\countFn -> map (\(a,_)->a) (countFn gameTreeObj) ) (map countChildrenEndStates [(W X),D,(W O)])
                                      where gameTreeObj = gameTree player board
-- allEndStateGlobalCount O [[(P X),(P O),(P X)],[E,(P O),E],[(P X),E,E]]
-- countChildEndStates (W O) (gameTree O [[(P X),(P O),(P X)],[E,(P O),E],[(P X),E,E]])



combinationRule :: Num a => [[(a, b)]] -> [(a, b)]
combinationRule ls = map (\[(xWin,board),(draw,_),(oWin,_)]-> ((oWin+xWin+draw),board)) $ transpose ls


boardPositionDiff :: Board -> Board -> [Move]
boardPositionDiff a b = filter (\(x,y) -> a!!x!!y /= b!!x!!y) [(x,y) |x <- [0..2], y <- [0..2]]

minimax :: Player -> GameTree -> [Int]
minimax player (Node parent children) = map (minimax' player) children

-- By convention O is the maximizing agent, while X is the minimizing agent.
minimax' :: Player -> GameTree -> Int
-- The minimizer
minimax' X (Node parent children) = minimum $ map (minimax' O) children
-- The maximizer
minimax' O (Node parent children) = maximum $ map (minimax' X) children

-- Terminal "heuristic" values.
minimax' _ (Terminal (W O)) = 10
minimax' _ (Terminal (W X)) = -10
minimax' _ (Terminal D) = 0


-- minimax O (gameTree O [[(P O),(P X),(P O)],[(P X),(P O),(P X)],[E,E,E]])
-- minimax X (gameTree X [[(P O),(P X),(P O)],[(P X),(P O),(P X)],[E,E,E]])

-- minimax X (gameTree X initialState)
-- minimax O (gameTree O initialState)

-- minimax O (gameTree O [[(P O),(P X),(P O)],[(P X),(P O),(P X)],[E,E,E]])

-- minimax O (gameTree O [[(P O),(P X),(P O)],[(P X),(P O),(P X)],[E,E,E]])
-- minimax X (gameTree X [[(P O),(P X),(P O)],[(P X),(P O),(P X)],[E,(P O),E]])
-- nChildren $ gameTree X [[(P O),(P X),(P O)],[(P X),(P O),(P X)],[E,(P O),E]]

nChildren (Node parent children) = length children



-- boardHeuristic X [[E,E,E],[E,(P X),E],[E,E,E]]
-- getThrees [[E,E,E],[E,(P X),E],[E,E,E]]
-- boardHeuristic X [[(P O),E,E],[E,(P X),E],[E,E,E]]
-- getThrees [[(P O),E,E],[E,(P X),E],[E,E,E]]
-- boardHeuristic O [[(P O),E,E],[E,(P X),E],[E,E,E]]
-- boardHeuristic X [[(P O),E,E],[E,(P X),E],[E,E,E]]
-- getThrees [[E,E,E],[E,(P X),E],[E,E,E]]
-- getThrees [[(P X),E,E],[E,E,E],[E,E,E]]
-- getThrees [[E,E,E],[(P X),E,E],[E,E,E]]
