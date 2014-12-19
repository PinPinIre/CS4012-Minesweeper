module Board (Board, initBoard, boardCells, gameWon) where

import           Control.Lens          ( (^.) )
import           Cell                  ( Cell
                                       , initCell
                                       , mined
                                       , flagged
                                       , revealed
                                       , adjacentMines
                                       )
import           Data.Vector           ( Vector )
import qualified Data.Vector as Vector ( concat
                                       , replicate
                                       , toList
                                       )

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

gameWon :: Board -> Bool
gameWon (Board cells) = all (==True) (map checkState concatCells)
    where
        concatCells = Vector.toList $ Vector.concat $ Vector.toList cells
        checkState c = (c ^. flagged && c ^. mined) || (c ^. revealed && not (c ^. mined))
