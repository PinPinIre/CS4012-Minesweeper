module Main where

import Game
import Control.Monad.State
import System.Random

main :: IO ()
main = do
    rng <- newStdGen
    let minesweeper = initMinesweeper rng
    let finished = execState testRun minesweeper
    print finished

testRun :: State Minesweeper Status
testRun = do
    _ <- revealCell 1 2
    _ <- revealCell 2 7
    flagCell 5 8
