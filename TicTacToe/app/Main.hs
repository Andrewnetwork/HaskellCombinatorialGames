module Main where

import TicTacToe
import Symmetry 
import Analysis
import AI
import Data.Maybe

gameLoop board player = do input <- getLine
                           let nextMove = read input :: (Int,Int)
                           if (isValidMove player board nextMove)
                              then do
                                let newBoard = move player board nextMove
                                let termState = terminalState newBoard
                                print (newBoard)
                                
                                if isJust termState
                                    then 
                                        case (fromJust termState) of
                                            (W x) -> print(show(x)++" wins!")
                                    else do
                                        let aiPlay = move O newBoard (pickSelfishMove O (gameTree O newBoard))
                                        print(aiPlay)
                                        gameLoop aiPlay X
                              else do
                                print("Invalid move.")
                                gameLoop board player
main :: IO ()
main = gameLoop initialState X
