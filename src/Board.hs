module Board where

import           Cell                  (Cell, initCell)
import           Data.Vector           (Vector)
import qualified Data.Vector as Vector (empty, replicate, toList)

type Pos = (Int, Int)

data Board = Board
    { boardMines :: Vector Pos
    , boardCells :: Vector (Vector Cell)
    }

instance Show Board where
    show = unlines . map (concatMap show . Vector.toList) . Vector.toList . boardCells

initBoard :: Int -> Int -> Board
initBoard w h = Board { boardMines = Vector.empty
                      , boardCells = board
                      }
    where
        rows  = Vector.replicate h initCell
        board = Vector.replicate w rows
