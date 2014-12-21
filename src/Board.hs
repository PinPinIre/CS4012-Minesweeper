{-# LANGUAGE TemplateHaskell #-}

module Board where

import Cell
import Control.Lens
import Data.Vector (Vector)
import qualified Data.Vector as Vector (fromList, toList)

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
calculateAdjacency board = Vector.fromList $ map Vector.fromList adjacent
    where
        filterMines = filter (^. mined)
        listBoard = map Vector.toList $ Vector.toList board
        mines = concatMap filterMines listBoard
        adjacent = map (map (countAdjacentMines mines)) listBoard

countAdjacentMines :: [Cell] -> Cell -> Cell
countAdjacentMines [] c = c
countAdjacentMines (m:mines) c
    | isAdjacent c m = countAdjacentMines mines (c & adjacentMines +~ 1)
    | otherwise      = countAdjacentMines mines c

isAdjacent :: Cell -> Cell -> Bool
isAdjacent c1 c2 = dist' == 1
    where
        x' = c1 ^. xpos - c2 ^. xpos
        y' = c1 ^. ypos - c2 ^. ypos
        dist = fromIntegral (x'*x' + y'*y')
        dist' = (floor :: Double -> Int) $ (sqrt :: Double -> Double) dist
