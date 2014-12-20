{-# LANGUAGE TemplateHaskell #-}

module Cell where

import Control.Lens

data Cell = Cell
    { _mined :: Bool
    , _flagged :: Bool
    , _revealed :: Bool
    , _adjacentMines :: Int
    }

makeLenses ''Cell

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
