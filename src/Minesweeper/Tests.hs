module Tests where

import Board
import Cell
import Game

import Control.Monad.State
import Data.Vector (Vector)
import Test.QuickCheck
import System.Random
import Control.Lens
import qualified Data.Vector as Vector

test :: Bool -> IO()
test True = do
    quickCheck prop_test

test False = return ()

--test proposition
prop_test :: Int -> Int -> Bool
prop_test x y = (x + y) == (y + x)

--check if number of mines added is same as placed on board
prop_minesadded :: Int -> IO Bool
prop_minesadded mineNumber = do
    board <- generateBoard
    let mines = minedCells board
    let result = (mineNumber == mines)
    return result

generateBoard :: IO Board
generateBoard = do
    rng <- newStdGen
    let board = initBoard 20 20 10 rng
    return board

minedCells :: Board -> Int
minedCells b = length mcells
    where
        boardCells = b ^. cells
        mcells = concat $ Vector.toList $ Vector.map (Vector.toList . Vector.filter _mined) boardCells