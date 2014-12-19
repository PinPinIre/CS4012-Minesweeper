module Main where

import Board (initBoard, gameWon)

main :: IO ()
main = do
    let board = initBoard 20 20
    let won = gameWon board
    print board
    print won
