module Main where

import Board (initBoard, gameStatus)

main :: IO ()
main = do
    let board = initBoard 20 20
    let won = gameStatus board
    print board
    print won
