{-# LANGUAGE TemplateHaskell #-}

module Cell (Cell, initCell) where

import Control.Lens

data Cell = Cell
    { _mined :: Bool
    , _flagged :: Bool
    , _revealed :: Bool
    , _adjacentMines :: Int
    }

instance Show Cell where
    show s
        | _flagged s  = "[ F ]"
        | _revealed s = "[ - ]"
        | _mined s    = "[ * ]"
        | otherwise   = "[   ]"

initCell :: Cell
initCell = Cell { _mined = False
                , _revealed = False
                , _flagged = False
                , _adjacentMines = 0
                }

makeLenses ''Cell
