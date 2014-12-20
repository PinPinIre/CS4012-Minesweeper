module Main where

import Game

main :: IO ()
main = do
    let minesweeper = initMinesweeper
    print minesweeper
