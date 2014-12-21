{-# LANGUAGE TemplateHaskell #-}

module Board where

import Cell
import Control.Lens
import Data.Vector (Vector)
import qualified Data.Vector as Vector (concatMap, filter, fromList, head, map, null, tail, toList)

data Board = Board { _cells :: Vector (Vector Cell) }

makeLenses ''Board

instance Show Board where
    show = unlines . map (concatMap show . Vector.toList) . Vector.toList . _cells

initBoard :: Int -> Int -> Board
initBoard w h = Board adjacencyBoard
    where
        board = Vector.fromList [Vector.fromList [initCell y x | x <- [0..(w - 1)]] | y <-[0..(h - 1)]]
        adjacencyBoard = calculateAdjacency board

calculateAdjacency :: Vector (Vector Cell) -> Vector (Vector Cell)
calculateAdjacency board = adjacent
    where
        filterMines = Vector.filter _mined
        mines = Vector.concatMap filterMines board
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
