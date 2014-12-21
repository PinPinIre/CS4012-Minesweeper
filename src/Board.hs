{-# LANGUAGE TemplateHaskell #-}

module Board where

import Cell
import Control.Lens
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import System.Random

data Board = Board { _cells :: Vector (Vector Cell) }

makeLenses ''Board

instance Show Board where
    show = unlines . map (concatMap show . Vector.toList) . Vector.toList . _cells

initBoard :: Int -> Int -> Int -> StdGen -> Board
initBoard w h mines rng = Board adjacencyBoard
    where
        board = Vector.fromList [Vector.fromList [initCell y x | x <- [0..(w - 1)]] | y <- [0..(h - 1)]]
        minedBoard = addMines 10 w h rng board
        adjacencyBoard = calculateAdjacency minedBoard

addMines :: Int -> Int -> Int -> StdGen -> Vector (Vector Cell) -> Vector (Vector Cell)
addMines 0 _ _ _ b = b
addMines count w h rng b = addMines (count-1) w h newRng' newBoard
    where
        (x, newRng) = randomR (0, w-1) rng
        (y, newRng') = randomR (0, h-1) newRng
        newBoard = b & (element y . element x . mined) .~ True

calculateAdjacency :: Vector (Vector Cell) -> Vector (Vector Cell)
calculateAdjacency board = adjacent
    where
        onlyMinesFilter = Vector.filter _mined
        mines = Vector.concatMap onlyMinesFilter board
        adjacent = Vector.map (Vector.map (countAdjacentMines mines)) board

countAdjacentMines :: Vector Cell -> Cell -> Cell
countAdjacentMines mines c
    | Vector.null mines = c
    | isAdjacent c m    = countAdjacentMines t (c & adjacentMines +~ 1)
    | otherwise         = countAdjacentMines t c
    where
        m = Vector.head mines
        t = Vector.tail mines

isAdjacent :: Cell -> Cell -> Bool
isAdjacent c1 c2 = floored == 1
    where
        x' = c1 ^. xpos - c2 ^. xpos
        y' = c1 ^. ypos - c2 ^. ypos
        dist = (sqrt :: Double -> Double) $ fromIntegral (x'*x' + y'*y')
        floored = (floor :: Double -> Int) dist
