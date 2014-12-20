{-# LANGUAGE TemplateHaskell #-}

module Minesweeper (initMinesweeper, gameStatus) where

import Board
import Cell
import Control.Lens
import qualified Data.Vector as Vector (concat, toList)

type Pos = (Int, Int)

data Status = Won | Lose | Move deriving (Show, Eq)

data Minesweeper = Minesweeper
    { _board :: Board
    , _remainingFlags :: Int
    }

makeLenses ''Minesweeper

instance Show Minesweeper where
    show = show . (^. board)

initMinesweeper :: Minesweeper
initMinesweeper = Minesweeper { _board = initBoard 20 20
                              , _remainingFlags = 10
                              }

gameStatus :: Minesweeper -> Status
gameStatus m
    | all (==Won) cellStates = Won
    | Lose `elem` cellStates = Lose
    | otherwise = Move
    where
        cells = boardCells $ m ^. board
        concatCells = Vector.toList $ Vector.concat $ Vector.toList cells
        checkCellState c
            | c ^. flagged && c ^. mined = Won
            | c ^. revealed && c ^. mined = Lose
            | otherwise = Move

        cellStates = map checkCellState concatCells
