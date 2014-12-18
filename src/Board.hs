module Board (Board, boardCells, initBoard) where

import           Cell                  (Cell, initCell)
import           Data.Vector           (Vector)
import qualified Data.Vector as Vector (replicate, toList)

data Board = Board { boardCells :: Vector (Vector Cell) }

instance Show Board where
    show = unlines . map (concatMap show . Vector.toList) . Vector.toList . boardCells

initBoard :: Int -> Int -> Board
initBoard w h = Board { boardCells = board }
    where
        rows  = Vector.replicate h initCell
        board = Vector.replicate w rows
