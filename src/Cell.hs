{-# LANGUAGE TemplateHaskell #-}

module Cell where

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
