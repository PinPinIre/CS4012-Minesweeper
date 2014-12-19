module Main where

import Minesweeper

main :: IO ()
main = do
    let minesweeper = initMinesweeper
    let won = gameStatus minesweeper
    print minesweeper
    print won
