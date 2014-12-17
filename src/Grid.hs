module Grid (Grid, initGrid) where

import Square    (Square, initSquare)
import Data.List (intercalate)

data Grid = Grid { width :: Int
                 , height :: Int
                 , grid :: [[Square]]
                 }

instance Show Grid where
    show g = intercalate "\n" showRows
        where
            showRow = foldr (\s1 s2 -> show s1 ++ s2) ""
            showRows = map showRow $ grid g

initGrid :: Int -> Int -> Grid
initGrid w h = Grid { width = w
                    , height = h
                    , grid = board
                    }
    where
        rows  = replicate h initSquare
        board = replicate w rows

