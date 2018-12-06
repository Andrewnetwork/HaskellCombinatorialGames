-- tictactoe.hs
-- December 2018
-- Andrew Ribeiro 

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module TicTacToe where
-- Rules of the game: 
-- > Board is initially empty.
-- > A starting player is chosen at random. 
-- > The player who "owns" the turn may place his piece in any empty cell. 
-- > After a player plays his piece, the turn enters the end turn stage. 
-- > In the end turn stage either the game terminates (win/draw) or next player is passed ownership of the board. 
-- > A win condition for player z=(x|y) is reached when there are three z's along a row, 
--   column, diagonal, or counter-diagonal. 
-- > A draw condition is reached when 9 turns have passed and no win condition has been satisfied. 
import Data.List.Split
import Data.List (transpose,elemIndex,nub,intercalate)
import Data.Maybe
import Data.Monoid
import System.Random

-- #### Helper Functions ####
printBoard :: Board -> IO ()
printBoard boardState = putStrLn.init $ concatMap (\x->x++"\n") (map show boardState)

formatBoard :: [[Cell]] -> String
formatBoard []   = "┌─┐\n└─┘\n"
formatBoard [[]] = "┌─┐\n└─┘\n"
formatBoard xxs  = (++ bot) $ concat $ zipWith (\a b -> unlines [a, b]) (top : replicate rowC mid) rows
  where
    rowC   = pred . length $ xxs
    colC   = pred . length . head $ xxs
    top    = "┌" ++ repC "─┬" ++ "─┐"
    mid    = "├" ++ repC "─┼" ++ "─┤"
    bot    = "└" ++ repC "─┴" ++ "─┘"
    repC  = concat . replicate colC
    rows   = (++ "|") . ('|' :) . intercalate "|" . ((\x -> if x == E then " " else show x) <$>) <$> xxs

prettyPrint :: [[Cell]] -> IO ()
prettyPrint = putStrLn . formatBoard
--prettyPrint [[(P X),E,E],[E,(P X),E],[E,E,(P X)]]
-- credit: newmaidumosa

-- printBoard [[(P X),E,E],[E,(P X),E],[E,E,(P X)]]

-- #### Data Structures ####
-- Cells, Boards, Turns, Games. 
-- Boards ⊂ Cell X Cell X ..6.. X Cell 
data Player = X | O deriving Show 
data Cell = E | P Player
data EndState = D | W Player deriving (Show,Eq)
data GameTree = Terminal EndState | Node Board [GameTree] deriving Show
data Game = Game [Board] EndState 
type Move = (Int,Int)
type Board = [[Cell]]


instance {-# OVERLAPPING #-} Show Board where
    show x = formatBoard x

instance Eq Cell where
    E == E = True
    P a == P b = a == b
    E == P a = False 
    P a == E = False   

instance Show Cell where 
    show E = "E"
    show (P x) = show x

instance Eq Player where
    X == O = False 
    O == X = False 
    X == X = True 
    O == O = True 

initialState = [[E,E,E],[E,E,E],[E,E,E]]
-- We can represent a board state in various ways. For example, the following would represent an empty board:
-- (1) [E,E,E,E,E,E,E,E,E]
-- (2) [[E,E,E],[E,E,E],[E,E,E]]
-- We will be using the nested representation (2).  

-- #### Game Functions ####
insert :: (Eq t1, Num t1) => t2 -> [t2] -> t1 -> [t2]
insert e [] pos = []
insert e (x:xs) pos
    | pos == 0 = e:xs
    | otherwise = x:(insert e (xs) (pos-1))
-- insert 2 [1,1,1,1,1,1] 3 

collapseIndex :: Num a => (a, a) -> a -> a
collapseIndex multIndex rowLen = (fst multIndex)*rowLen+(snd multIndex)

move :: Player -> Board -> Move -> Board
move player boardState position = chunksOf 3 (insert 
                                                (playerToCell player) 
                                                (mconcat boardState) 
                                                (collapseIndex position 3))
-- move X initialState (2,2)
-- printBoard (move X initialState (2,2))
-- printBoard (move O (move X initialState (2,1)) (0,0))
-- printBoard (move O (move O initialState (2,1)) (0,0))

-- We need to account for three conditions in determining if a move is valid. 
-- (1) The number of player pieces differs by no more than one. (captures alternating turn rule)
-- (2) The game is not in a terminal state. (not a win/draw already)
-- (3) A player piece can only replace an empty cell, not another player. (no obstructions)

isValidMove :: Player -> Board -> Move -> Bool
isValidMove player boardState position = foldl (&&) True (validateMove player boardState position)

validateMove :: Player -> Board -> Move -> [Bool]
validateMove player boardState position = [pieceCountCond,terminalCond,emptyCellCond]
                                          where proposedBoard = move player boardState position
                                                pieceCountCond = abs((countPlayer player boardState)+1 - (countPlayer (otherPlayer player) boardState)) <= 1
                                                terminalCond = not (isTerminalState boardState) 
                                                emptyCellCond = not (isPlayerLoc boardState position)
-- validateMove O initialState (0,0)
speakAboutMove :: Player -> Board -> Move -> [Char]
speakAboutMove player boardState position = (if not (validationConds !! 0) then "It's the other player's turn! " else "")++
                                            (if not (validationConds !! 1) then "The game is over man! " else "")++
                                            (if not (validationConds !! 2) then "There's a piece there! " else "")
                                            where validationConds = validateMove player boardState position
-- speakAboutMove O initialState (0,0)
-- speakAboutMove O [[(P O),E,E],[E,E,E],[E,E,E]] (0,0)
-- speakAboutMove O [[(P O),E,E],[E,(P O),E],[E,E,(P O)]] (2,2)


--playerTurn boardState =
otherPlayer X = O
otherPlayer O = X

-- [isPieceCountValid ,isTerminalState,isPlayerLoc]

isTerminalState :: Board -> Bool
isTerminalState boardState = case (terminalState boardState) of
                                Nothing -> False 
                                _ -> True
-- isTerminalState [[(P O),E,E],[E,(P O),E],[E,E,(P X)]]
-- isTerminalState [[(P O),E,E],[E,(P O),E],[E,E,(P O)]]

isPlayerLoc :: Board -> Move -> Bool
isPlayerLoc boardState move = ((concat boardState)!!(collapseIndex move 3)) /= E
--isPlayerLoc [[(P O),E,E],[E,(P O),E],[E,E,(P X)]] (0,0)

isPieceCountValid :: Board -> Bool
isPieceCountValid boardState = (abs ((countPlayer X boardState)-(countPlayer O boardState))) <= 1
-- isPieceCountValid [[(P X),E,E],[E,(P X),E],[E,E,(P X)]] -> False
-- isPieceCountValid initialState
-- isPieceCountValid [[(P O),E,E],[E,(P O),E],[E,E,(P X)]] 

countCell :: Cell -> Board -> Int
countCell cell boardState = length $ filter (cell==) (concat boardState) 
-- countCell (P X) [[(P X),E,E],[E,(P X),E],[E,E,(P X)]]

countPlayer :: Player -> Board -> Int
countPlayer player boardState = countCell (playerToCell player) boardState
-- countPlayer X [[(P X),E,E],[E,(P X),E],[E,E,(P X)]]

-- validMoves player boardState = map (\x-> move player boardState) [(x,y) | x<-[0,1,2],y<-[0,1,2]]

-- [[0,1,2],[3,4,5],[6,7,8]] -> [0,4,8]
-- [[0,1,2,3],[4,5,6,7],[8,9,10,11],[12,13,14,15]] -> [0,5,10,15]
-- Problem: Given a list of lists, produce a list where the first element 
--          of the list is the first element of the first list, the second 
--          element the second element of the second list, and so on.
incGrabber :: [[a]] -> Int -> [a] 
incGrabber [] n = []
incGrabber (x:xs) n = (x!!n):(incGrabber xs (n+1))
-- incGrabber [[0,1,2,3],[4,5,6,7],[8,9,10,11],[12,13,14,15]]

isAllPlayer :: [Cell] -> Bool
isAllPlayer [a,b,c] = (a == b) && (b == c) && a /= E 

cellToPlayer :: Cell -> Player
cellToPlayer (P x) = x
cellToPlayer E = error "Cannot convert E to a player."

playerToCell :: Player -> Cell
playerToCell X = P X
playerToCell O = P O  

-- #### Win Conditions ####
whoWonDiag :: Board -> Maybe Player
whoWonDiag boardState
    | isAllPlayer res = Just (cellToPlayer (head res))
    | otherwise = Nothing
    where res = incGrabber boardState 0
-- whoWonDiag [[(P X),E,E],[E,(P X),E],[E,E,(P X)]]

whoWonCounterDiag :: Board -> Maybe Player
whoWonCounterDiag boardState = whoWonDiag (reverse boardState)
-- whoWonCounterDiag [[(P X),E,E],[E,(P X),E],[E,E,(P X)]]
-- whoWonCounterDiag [[E,E,(P X)],[E,(P X),E],[(P X),E,E]]

whoWonRow :: Board -> Maybe Player
whoWonRow boardState = 
    case (filter isAllPlayer boardState) of
        [] -> Nothing
        (x:xs) -> Just (cellToPlayer ( head x))
-- whoWonRow [[(P X),(P X),(P X)],[(P X),(P X),(P O)],[(P X),(P X),(P O)]]
-- whoWonRow [[(P X),E,E],[E,(P X),E],[E,E,(P X)]]
-- whoWonRow (transpose [[(P X),(P X),(P X)],[(P X),(P X),(P O)],[(P X),(P X),(P O)]])

whoWonColumn :: Board -> Maybe Player
whoWonColumn boardState = whoWonRow (transpose boardState)
-- whoWonColumn [[(P X),(P X),(P X)],[(P X),(P X),(P O)],[(P X),(P X),(P O)]]

whoWon :: Board -> Maybe Player
whoWon boardState = getFirst (mconcat (map First res))
    where res = map ($ boardState) [whoWonDiag,whoWonCounterDiag,whoWonRow,whoWonColumn]
-- whoWon [[(P X),(P X),(P X)],[(P X),(P X),(P O)],[(P X),(P X),(P O)]]
-- whoWon [[(P X),(P X),(P X)],[(P O),(P O),(P X)],[(P O),(P O),(P X)]]

isBoardFull :: Board -> Bool
isBoardFull boardState = all (/= E) (concat boardState)
-- isBoardFull [[(P X),(P X),(P X)],[(P O),(P O),(P X)],[(P O),(P O),(P X)]]
-- isBoardFull [[(P X),E,E],[E,(P X),E],[E,E,(P X)]]

terminalState :: Board -> Maybe EndState
terminalState boardState 
    | winState /= Nothing = Just (W (fromJust winState) )
    | boardFull && (winState == Nothing) = Just D 
    | otherwise = Nothing 
    where winState = whoWon boardState
          boardFull = isBoardFull boardState
-- terminalState [[(P X),E,E],[E,(P X),E],[E,E,(P X)]]
-- terminalState initialState 

validMoves :: Player -> Board -> [Move]
validMoves player boardState = [(x,y) | x<-[0,1,2], y<-[0,1,2], isValidMove player boardState (x,y)]
-- validMoves O initialState

--makeRandomMove player boardState  = do let moves = validMoves player boardState
                                     -- idx <- randomRIO(0,length(moves))
                                      --move player boardState (moves !! idx) 

                                      
pickFirstMove player boardState = move player boardState (head (validMoves player boardState))
-- pickFirstMove X (pickFirstMove O initialState)
-- pickFirstMove O (pickFirstMove O initialState)

--testBank = do x <- traverse (\z -> randomRIO(1::Int,z)) [1,2,3]
--return x        
                  
playGame initPlayer boardState = case (terminalState boardState) of
                                    Nothing -> newBoard:(playGame (otherPlayer initPlayer) newBoard)
                                    _ -> []
                                    where newBoard = (pickFirstMove initPlayer boardState)

-- terminalState (last (playGame O initialState))

-- [[O,X,O], 
--  [X,O,X],
--  [O,E,E]]
 -- pickFirstMove O (pickFirstMove X (pickFirstMove O (pickFirstMove X (pickFirstMove O (pickFirstMove X (pickFirstMove O initialState))))))
 -- validMoves X (pickFirstMove O (pickFirstMove X (pickFirstMove O initialState)))
 -- validMoves O (pickFirstMove X (pickFirstMove O (pickFirstMove X (pickFirstMove O (pickFirstMove X (pickFirstMove O initialState))))))
 -- validMoves O [[(P O),(P X),(P O)],[(P X),(P O),(P X)],[E,E,E]]
 -- speakAboutMove O [[(P O),(P X),(P O)],[(P X),(P O),(P X)],[E,E,E]] (2,0)
 -- terminalState [[(P O),(P X),(P O)],[(P X),(P O),(P X)],[E,E,E]]
 -- validateMove O [[(P O),(P X),(P O)],[(P X),(P O),(P X)],[E,E,E]] (2,0)

gameOutcomes :: Player -> Board -> [EndState]
gameOutcomes player [] = [] 
gameOutcomes player boardState 
    | isTerminalState boardState = maybeToList $ terminalState boardState -- Maybe EndState 
    | otherwise = concatMap (gameOutcomes (otherPlayer player)) boards 
    where boards = makeAllMoves player boardState

gameTree :: Player -> Board -> GameTree
gameTree player boardState 
    | isTerminalState boardState = Node boardState [Terminal (fromJust $ (terminalState boardState))]
    | otherwise = Node boardState children 
    where boards = makeAllMoves player boardState 
          children = map (gameTree (otherPlayer player)) boards
-- x = gameTree O initialState 

gameTreeOutcomes :: GameTree -> [EndState]
gameTreeOutcomes (Terminal x) = [x]
gameTreeOutcomes (Node parent children) = concatMap gameTreeOutcomes children
-- x = gameTreeOutcomes (gameTree O initialState)


-- Given the tic-tac-toe game tree, how many unique draw states are there? 

findTerminalBoards :: Board -> EndState -> GameTree -> [Board]
findTerminalBoards parentNode endState (Terminal x)
    | x == endState = [parentNode] 
    | otherwise = []
findTerminalBoards parentNode endState (Node parent children) = concatMap (findTerminalBoards parent endState) children 

-- x = findTerminalBoards initialState D (gameTree O initialState)
-- z = nub x 
-- mapM_ putStrLn ( map (unlines . fmap show)  z )
-- mapM_ putStrLn ( map (unlines . fmap show)  ( nub x ) )

-- Node initialState (map (\x-> Node x [Terminal D]) (makeAllMoves O initialState)
-- Node initialState (map (\x-> Node x [Terminal D]) (makeAllMoves O initialState))
--  Node boardState (map (\x-> Node x [Terminal D]) (concatMap (gameTree (otherPlayer player)) boards)
-- x = gameOutcomes O initialState 
-- length x
-- length (filter (D==) x) 
-- length (filter ((W O)==) x) 
-- length (filter ((W X)==) x) 
-- (length x) == (length (filter (D==) x)) + (length (filter ((W O)==) x)) + (length (filter ((W X)==) x) )
-- length (filter ((W O)==) x) 
-- length (filter ((W X)==) x) 

-- nub (filter (D==) x) 


-- Timer: :set +s
makeAllMoves :: Player -> Board -> [Board]
makeAllMoves player boardState = (map (move player boardState) (validMoves player boardState))

--map (\x -> move O initialState x) (validMoves O initialState)
-- q