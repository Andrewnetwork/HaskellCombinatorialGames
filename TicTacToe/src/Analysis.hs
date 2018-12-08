-- Analysis.hs
-- December 2018
-- Andrew Ribeiro 

module Analysis where
import TicTacToe 
import Symmetry
import Data.List (nub)
import Data.List.Split

--drawBoards = nub $ findTerminalBoards initialState D (gameTree O initialState)
-- map symmetries drawBoards
-- getSymFlags$last$(map makeSym drawBoards)

-- symPath initialState 
isValidBoard :: Board -> Bool
isValidBoard board = case threeInstances of 
                        [] -> isPieceCountValid board
                        (x:xs) -> pieceCountCond && noMultipleWins && ((countPlayer x board)>=(countPlayer (otherPlayer x) board))
                        where noMultipleWins = (length $ threeInstances) <= 1
                              threeInstances = playerThrees board
                              pieceCountCond = isPieceCountValid board
                           