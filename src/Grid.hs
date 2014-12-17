module Grid (Grid, initGrid) where

import Square (Square, initSquare)

data Grid = Grid { width :: Int
                 , height :: Int
                 , grid :: [[Square]]
                 }

instance Show Grid where
    show = unlines . map (concatMap show) . grid

initGrid :: Int -> Int -> Grid
initGrid w h = Grid { width = w
                    , height = h
                    , grid = board
                    }
    where
        rows  = replicate h initSquare
        board = replicate w rows
