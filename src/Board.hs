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

data Status = Won | Lose | Move

instance Show Board where
    show = unlines . map (concatMap show . Vector.toList) . Vector.toList . boardCells

initBoard :: Int -> Int -> Board
initBoard w h = Board board
    where
        rows  = Vector.replicate h initCell
        board = Vector.replicate w rows

boardCells :: Board -> Vector (Vector Cell)
boardCells (Board cells) = cells

gameStatus :: Board -> Status
gameStatus (Board cells)
    | all (==Won) cellStates = Won
    | Lose `elem` cellStates = Lose
    | otherwise = Move
    where
        concatCells = Vector.toList $ Vector.concat $ Vector.toList cells
        cellStates = map checkCellState concatCells

checkCellState :: Cell
checkCellState c
    | (c ^. flagged && c ^. mined) || (c ^. revealed && not (c ^. mined)) = Won
    | c ^. revealed && c ^. mined = Lose
