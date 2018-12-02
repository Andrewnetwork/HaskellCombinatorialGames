-- tictactoe.hs
-- December 2018
-- Andrew Ribeiro 

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
import Data.List (transpose,elemIndex)
import Data.Maybe

-- #### Data Structures ####
-- Cells, Boards, Turns, Games. 
-- Boards ⊂ Cell X Cell X ..6.. X Cell 
data Player = X | O deriving Show 
data Cell = E | P Player deriving Show 
data EndState = D | W Player deriving Show 
type Board = [[Cell]]

instance Eq Cell where
    E == E = True
    P a == P b = a == b
    E == P a = False 
    P a == E = False   

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

-- #### Functions ####

insert :: (Eq t1, Num t1) => t2 -> [t2] -> t1 -> [t2]
insert e [] pos = []
insert e (x:xs) pos
    | pos == 0 = e:xs
    | otherwise = x:(insert e (xs) (pos-1))
-- insert 2 [1,1,1,1,1,1] 3 

move :: Cell -> [Cell] -> Int -> [Cell]
move player boardState position = insert player boardState position
-- move (P X) initialState 2
-- move (P O) (move (P X) initialState 2) 0
 
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
whoWon boardState
    | all (Nothing== ) res= Nothing
    | otherwise = head (filter (\x -> Just O == x || Just X == x) res)
    where res = map (\x -> x boardState) [whoWonDiag,whoWonCounterDiag,whoWonRow,whoWonColumn]
-- whoWon [[(P X),(P X),(P X)],[(P X),(P X),(P O)],[(P X),(P X),(P O)]]
-- whoWon [[(P X),(P X),(P X)],[(P O),(P O),(P X)],[(P O),(P O),(P X)]]

isBoardFull :: Foldable t => [t Cell] -> Bool
isBoardFull boardState = not (foldl (||) False (map (any (E==)) boardState))
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

-- #### Printing Functions ####
printBoard boardState = concatMap (\x->x++"\n") (map show boardState)
-- printBoard [[(P X),E,E],[E,(P X),E],[E,E,(P X)]]