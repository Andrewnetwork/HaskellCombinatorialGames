-- AI.hs
-- Andrew Ribeiro 
-- December 2018

module AI where
import TicTacToe
import Data.Maybe
import System.Random
import Data.List 

data GameTree = Terminal EndState | Node Board [GameTree] deriving Show
data Game = Game [Board] EndState 

findTerminalBoards :: Board -> EndState -> GameTree -> [Board]
findTerminalBoards parentNode endState (Terminal x)
    | x == endState = [parentNode] 
    | otherwise = []
findTerminalBoards parentNode endState (Node childParent children) = concatMap (findTerminalBoards childParent endState) children 

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

-- Create a game tree given a current player and board state. 
gameTree :: Player -> Board -> GameTree
gameTree player boardState 
    | isTerminalState boardState = Node boardState [Terminal (fromJust $ (terminalState boardState))]
    | otherwise = Node boardState children 
    where boards = makeAllMoves player boardState 
          children = map (gameTree (otherPlayer player)) boards

-- Given a game tree, produce a list of all end states. 
gameTreeOutcomes :: GameTree -> [EndState]
gameTreeOutcomes (Terminal x) = [x]
gameTreeOutcomes (Node parent children) = concatMap gameTreeOutcomes children

--makeRandomMove player boardState  = do let moves = validMoves player boardState
                                     -- idx <- randomRIO(0,length(moves))
                                      --move player boardState (moves !! idx) 

--pathsToEndState :: EndState -> GameTree -> [GameTree] 
--pathsToEndState endState (Terminal X) = X == endState 
--pathsToEndState endState (Node parent children) = filter (isEndStatePath endState) children   


countChildEndStates :: EndState -> GameTree -> Int
countChildEndStates endState (Terminal x)
    | x == endState = 1
    | otherwise = 0
countChildEndStates endState (Node parent children) =  sum (map (countChildEndStates endState) children)
-- countChildEndStates (W O) (gameTree O initialState)

countChildrenEndStates :: EndState -> GameTree -> [(Int,GameTree)]
countChildrenEndStates endState (Node parent children) = zip (map (countChildEndStates endState) children) children

pickSelfishMove :: Player -> GameTree -> Move
pickSelfishMove player (Node parent children) = head$boardPositionDiff parent (getParent$snd$last$sortedCounts)
                                                where countTups = countChildrenEndStates (W player) (Node parent children) 
                                                      sortedCounts = (sortBy (\(a,_)(b,_)->compare a b) countTups)

getParent :: GameTree -> Board 
getParent (Node parent _) = parent 

-- gameTree O initialState 
-- gameTree X [[(P X),(P O),(P O)],[(P O),(P X),E],[E,E,E]]
-- countChildEndStates (W X) (gameTree X [[(P X),(P O),(P O)],[(P O),(P X),E],[E,E,E]])
-- countChildrenEndStates (W X) (gameTree X [[(P X),(P O),(P O)],[(P O),(P X),E],[E,E,E]])
--[2,2,2,1] (W X)
--[4,2,2,0] D
--[0,2,2,0] (W O)

-- countChildrenEndStates (W X) (gameTree X [[(P X),(P O),(P O)],[(P O),E,(P X)],[E,E,E]])
-- [2,0,0,2] (W X)
-- [4,4,4,0] D
-- [0,2,2,4] (W O)

-- countChildrenEndStates (W X) (gameTree X initialState)
-- Problem: Given board a and board b, determine where the difference is.
-- [[E,E,E],[E,E,E],[E,E,E]] [[(P X),E,E],[E,E,E],[E,E,E]] -> [(0,0)]
-- [[E,E,E],[E,E,E],[E,E,E]] [[E,E,E],[E,E,E],[E,E,(P X)]] -> [(2,2)]
-- [[E,E,E],[E,E,E],[E,E,E]] [[(P X),E,E],[E,E,E],[E,E,(P X)]] -> [(0,0),(2,2)]

boardPositionDiff :: Board -> Board -> [Move]
boardPositionDiff a b = filter (\(x,y) -> a!!x!!y /= b!!x!!y) [(x,y) |x <- [0..2], y <- [0..2]]
