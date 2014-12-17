module Grid (Grid, initGrid) where

import Square    (Square, initSquare)
import Data.List (intercalate)

data Grid = Grid { width :: Int
                 , height :: Int
                 , grid :: [[Square]]
                 }

instance Show Grid where
    show g = intercalate "\n" rows
        where
            showRow = foldr (\s acc -> show s ++ acc) ""
            rows = map showRow $ grid g

initGrid :: Int -> Int -> Grid
initGrid w h = Grid { width = w
                    , height = h
                    , grid = board
                    }
    where
        rows  = replicate h initSquare
        board = replicate w rows

