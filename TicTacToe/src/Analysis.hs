-- Analysis.hs
-- December 2018
-- Andrew Ribeiro
-- Contains analysis of TicTacToe boards and player statistics.

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

-- Proof of minimax optimality for Tic-Tac-Toe.
