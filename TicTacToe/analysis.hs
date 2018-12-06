-- analysis.hs
-- December 2018
-- Andrew Ribeiro 

module TTTAnalysis where
import TicTacToe 
import Symmetry
import Data.List (nub)
import Data.List.Split

--drawBoards = nub $ findTerminalBoards initialState D (gameTree O initialState)
-- map symmetries drawBoards
-- getSymFlags$last$(map makeSym drawBoards)

-- symPath initialState 