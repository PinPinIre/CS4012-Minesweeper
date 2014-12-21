module Main where

import Game
import Control.Monad.State

main :: IO ()
main = do
    let minesweeper = initMinesweeper
    let (_, finished) = runState testRun minesweeper
    print finished

testRun :: State Minesweeper Status
testRun = do
    revealCell (1, 2)
    revealCell (2, 7)
    flagCell (5, 8)
