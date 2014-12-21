{-# LANGUAGE TemplateHaskell #-}

module Board where

import Cell
import Control.Lens
import Data.Vector (Vector)
import qualified Data.Vector as Vector (replicate, toList)

data Board = Board { _cells :: Vector (Vector Cell) }

makeLenses ''Board

instance Show Board where
    show = unlines . map (concatMap show . Vector.toList) . Vector.toList . view cells

initBoard :: Int -> Int -> Board
initBoard w h = Board board
    where
        rows  = Vector.replicate h initCell
        board = Vector.replicate w rows
