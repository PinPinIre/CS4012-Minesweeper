{-# LANGUAGE TemplateHaskell #-}

module Square (Square, initSquare) where

import Control.Lens

data Square = Square { _mined :: Bool
                     , _flagged :: Bool
                     , _revealed :: Bool
                     }

instance Show Square where
    show s
        | _flagged s  = "[ F ]"
        | _revealed s = "[ - ]"
        | _mined s    = "[ * ]"
        | otherwise   = "[   ]"

initSquare :: Square
initSquare = Square { _mined = False
                    , _revealed = False
                    , _flagged = False
                    }

makeLenses ''Square
