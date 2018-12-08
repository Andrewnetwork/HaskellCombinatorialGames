-- TicTacToe.hs
-- December 2018
-- Andrew Ribeiro 

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module TicTacToe where

import Data.List.Split
import Data.List (transpose,elemIndex,nub,intercalate)
import Data.Maybe
import Data.Monoid

-- #### Data Structures ####
data Player = X | O deriving Show 
data Cell = E | P Player
data EndState = D | W Player deriving (Show,Eq)
type Move = (Int,Int)
type Board = [[Cell]]

instance {-# OVERLAPPING #-} Show Board where
    show x = "\n"++formatBoard x

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

-- #### Helper Functions ####
printBoard :: Board -> IO ()
printBoard boardState = putStrLn.init $ concatMap (\x->x++"\n") (map show boardState)

-- credit: newmaidumosa
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
isValidMove player boardState position = (fst position <= 2) && (snd position <= 2) && (fst position >= 0) && (snd position >= 0) && (foldl (&&) True (validateMove player boardState position))

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

otherPlayer X = O
otherPlayer O = X

isTerminalState :: Board -> Bool
isTerminalState boardState = case (terminalState boardState) of
                                Nothing -> False 
                                _ -> True

isPlayerLoc :: Board -> Move -> Bool
isPlayerLoc boardState move = ((concat boardState)!!(collapseIndex move 3)) /= E

isPieceCountValid :: Board -> Bool
isPieceCountValid boardState = (abs ((countPlayer X boardState)-(countPlayer O boardState))) <= 1

countCell :: Cell -> Board -> Int
countCell cell boardState = length $ filter (cell==) (concat boardState) 


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
diagThrees :: Board -> [Player]
diagThrees boardState
    | isAllPlayer res = [cellToPlayer (head res)]
    | otherwise = []
    where res = incGrabber boardState 0
-- whoWonDiag [[(P X),E,E],[E,(P X),E],[E,E,(P X)]]

cDiagThrees :: Board -> [Player]
cDiagThrees boardState = diagThrees (reverse boardState)
-- whoWonCounterDiag [[(P X),E,E],[E,(P X),E],[E,E,(P X)]]
-- whoWonCounterDiag [[E,E,(P X)],[E,(P X),E],[(P X),E,E]]

rowThrees :: Board -> [Player]
rowThrees boardState = case (filter isAllPlayer boardState) of
                         [] -> []
                         x -> map cellToPlayer (map head x)

columnThrees :: Board -> [Player]
columnThrees boardState = rowThrees (transpose boardState)
-- whoWonColumn [[(P X),(P X),(P X)],[(P X),(P X),(P O)],[(P X),(P X),(P O)]]

playerThrees :: Board -> [Player]
playerThrees boardState = concatMap ($ boardState) [diagThrees,cDiagThrees,rowThrees,columnThrees]
-- whoWon [[(P X),(P X),(P X)],[(P X),(P X),(P O)],[(P X),(P X),(P O)]]
-- whoWon [[(P X),(P X),(P X)],[(P O),(P O),(P X)],[(P O),(P O),(P X)]]

isBoardFull :: Board -> Bool
isBoardFull boardState = all (/= E) (concat boardState)
-- isBoardFull [[(P X),(P X),(P X)],[(P O),(P O),(P X)],[(P O),(P O),(P X)]]
-- isBoardFull [[(P X),E,E],[E,(P X),E],[E,E,(P X)]]

terminalState :: Board -> Maybe EndState
terminalState boardState 
    | not.null $ winState = Just (W (head winState) )
    | boardFull && (null winState) = Just D 
    | otherwise = Nothing 
    where winState = playerThrees boardState
          boardFull = isBoardFull boardState
-- terminalState [[(P X),E,E],[E,(P X),E],[E,E,(P X)]]
-- terminalState initialState 

validMoves :: Player -> Board -> [Move]
validMoves player boardState = [(x,y) | x<-[0,1,2], y<-[0,1,2], isValidMove player boardState (x,y)]
-- validMoves O initialState
        
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



--findTerminalBoards parentNode endState (Terminal x)

-- isEndStatePath D (gameTree O initialState)

-- Given the tic-tac-toe game tree, how many unique draw states are there? 




-- findTerminalBoards [[(P O),(P X),(P O)],[(P X),(P O),(P X)],[E,E,E]] (W O) (gameTree O [[(P O),(P X),(P O)],[(P X),(P O),(P X)],[E,E,E]])
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

--map (\x -> move O initialState x) (validMoves O initialState)
-- q