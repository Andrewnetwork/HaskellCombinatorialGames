module Main where

import TicTacToe
import Symmetry
import Analysis
import AI
import Data.Maybe

-- O by convention is the AI player, while X is the human player.
gameLoop board player aiPolicy = do let termState = terminalState board
                                    if isJust termState
                                        then do
                                            case (fromJust termState) of
                                                (W x) -> print(show(x)++" wins!")
                                                D -> print("Draw!")
                                        else do
                                            if player == O
                                                then do
                                                    let aiPlay = move O board (aiPolicy O (gameTree O board))
                                                    print(aiPlay)
                                                    gameLoop aiPlay X aiPolicy
                                                else do
                                                    input <- getLine
                                                    let nextMove = read input :: (Int,Int)
                                                    let newBoard = move player board nextMove

                                                    if (isValidMove player board nextMove)
                                                        then do
                                                            print (newBoard)
                                                            gameLoop newBoard O aiPolicy
                                                        else do
                                                            print("Invalid move.")
                                                            gameLoop board player aiPolicy

main :: IO ()
main = gameLoop initialState X minimaxMove 
