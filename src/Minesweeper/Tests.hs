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
prop_minesadded :: Int -> Int -> Int -> IO Bool
prop_minesadded x y minesPlaced = do
    board <- generateBoard x y minesPlaced
    let minesFound = minedCells board
    let result = (minesPlaced == minesFound)
    return result

generateBoard :: Int -> Int -> Int-> IO Board
generateBoard x y minePlaced = do
    rng <- newStdGen
    let board = initBoard x y minePlaced rng
    return board

minedCells :: Board -> Int
minedCells b = length mcells
    where
        boardCells = b ^. cells
        mcells = concat $ Vector.toList $ Vector.map (Vector.toList . Vector.filter _mined) boardCells
