module Board (Board, initBoard, boardCells) where

import Cell
import Data.Vector (Vector)
import qualified Data.Vector as Vector (replicate, toList)

data Board = Board (Vector (Vector Cell))

instance Show Board where
    show = unlines . map (concatMap show . Vector.toList) . Vector.toList . boardCells

initBoard :: Int -> Int -> Board
initBoard w h = Board board
    where
        rows  = Vector.replicate h initCell
        board = Vector.replicate w rows

boardCells :: Board -> Vector (Vector Cell)
boardCells (Board cells) = cells
