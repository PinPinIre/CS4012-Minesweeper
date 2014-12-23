{-# LANGUAGE TemplateHaskell #-}

module Board where

import Cell

import Control.Lens
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import System.Random

data Board = Board
    { _cells :: Vector (Vector Cell)
    , _width :: Int
    , _height :: Int
    }

makeLenses ''Board

instance Show Board where
    show = unlines . map (concatMap show . Vector.toList) . Vector.toList . _cells

initBoard :: Int -> Int -> Int -> StdGen -> Board
initBoard w h mines rng = adjacencyBoard
    where
        genCells = [[initCell x y | y <- [0..(h - 1)]] | x <- [0..(w - 1)]]
        board = Board (Vector.fromList $ map Vector.fromList genCells) w h
        minedBoard = addMines mines rng board
        adjacencyBoard = calculateAdjacency minedBoard

addMines :: Int -> StdGen -> Board -> Board
addMines 0 _ board = board
addMines count rng board = addMines (count-1) newRng' newBoard
    where
        (x, newRng) = randomR (0, board ^. width - 1) rng
        (y, newRng') = randomR (0, board ^. height - 1) newRng
        newBoard = board & (cells . element y . element x . mined) .~ True

calculateAdjacency :: Board -> Board
calculateAdjacency board = board & cells .~ adjacent
    where
        boardCells = _cells board
        onlyMinesFilter = Vector.filter _mined
        mines = Vector.concatMap onlyMinesFilter boardCells
        adjacent = Vector.map (Vector.map (countAdjacentMines mines)) boardCells

countAdjacentMines :: Vector Cell -> Cell -> Cell
countAdjacentMines mines c
    | Vector.null mines = c
    | isAdjacent c m    = countAdjacentMines t (c & adjacentMines +~ 1)
    | otherwise         = countAdjacentMines t c
    where
        m = Vector.head mines
        t = Vector.tail mines

minedCells :: Board -> Int
minedCells b = length mcells
    where
        boardCells = b ^. cells
        mcells = concat $ Vector.toList $ Vector.map (Vector.toList . Vector.filter _mined) boardCells
