{-# LANGUAGE TemplateHaskell #-}

module Minesweeper.Cell where

import Control.Lens

data Cell = Cell
    { _xpos :: Int
    , _ypos :: Int
    , _mined :: Bool
    , _flagged :: Bool
    , _revealed :: Bool
    , _adjacentMines :: Int
    }

makeLenses ''Cell

instance Show Cell where
    show c
        | _flagged c           = "[ F ]"
        | _revealed c          = "[ - ]"
        | _mined c             = "[ * ]"
        | _adjacentMines c > 0 = "[ " ++ (show $ _adjacentMines c) ++ " ]"
        | otherwise            = "[   ]"

initCell :: Int -> Int -> Cell
initCell x y = Cell { _xpos = x
                    , _ypos = y
                    , _mined = False
                    , _revealed = False
                    , _flagged = False
                    , _adjacentMines = 0
                    }

isAdjacent :: Cell -> Cell -> Bool
isAdjacent c1 c2 = floored == 1
    where
        x' = c1 ^. xpos - c2 ^. xpos
        y' = c1 ^. ypos - c2 ^. ypos
        dist = (sqrt :: Double -> Double) $ fromIntegral (x'*x' + y'*y')
        floored = (floor :: Double -> Int) dist
