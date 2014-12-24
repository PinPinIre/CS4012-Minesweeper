module Minesweeper.Solver where

import Minesweeper.Board
import Minesweeper.Cell
import Minesweeper.Game

import Control.Lens
import Control.Monad.State
import Data.List (delete, nub)
import Data.Maybe
import qualified Data.Vector as Vector

-- Find all the revealed cells in the game
findAllRevealed :: Game -> [Cell]
findAllRevealed b = revealedCells
    --filter the board cells based on the revealed attribute
    where
        boardCells = b ^. board . cells
        revealedCells = concat $ Vector.toList $ Vector.map (Vector.toList . Vector.filter _revealed) boardCells

-- Find all the hidden cells in the game
findAllUnrevealed :: Game -> [Cell]
findAllUnrevealed b = revealedCells
    --filter the board cells based on the inverse of the revealed attribute
    where
        boardCells = b ^. board . cells
        revealedCells = concat $ Vector.toList $ Vector.map (Vector.toList . Vector.filter (not . _revealed)) boardCells

-- Filter cells which don't have bombs adjacent to it
filterZeroAdj ::  [Cell] -> [Cell]
filterZeroAdj = filter (\x -> _adjacentMines x == 0)

-- Find the co-ordinates of cells that are safe to reveal
findSafeSquares :: GameState [(Int, Int)]
findSafeSquares = do
    m <- get
    {-
        Find all of the cells on the board which have 0 mines adjacent to it
        For each of these cells find the adjacent tiles that have not been
        revealed. Filter for duplicates and concat to a flat list.
    -}
    let zeroCells = filterZeroAdj $ findAllRevealed m
    ss <- mapM getHiddenNeighbours zeroCells
    return $ nub $ concat ss

-- Find all of the hidden cells imediately adjacent to a particular cell
getHiddenNeighbours :: Cell -> GameState [(Int, Int)]
getHiddenNeighbours c = do
    m <- get
    -- Get neighbours and filter only the hidden cells
    let nc = getNeighbourCords (c ^. xpos) (c ^. ypos)
    filterM (\(x,y) -> do
        let result = m ^? board . cells . element x . element y . revealed
        case result of
            Nothing -> return False
            _ -> return (not $ fromJust result)) nc

-- Find all the cells that are safe to flag as containing mines
findFlagSquares :: GameState [(Int, Int)]
findFlagSquares = do
    m <- get
    {-
        Get a list of cells that have not been revealed and filter out the ones
        which cannot contain a mine with.
        Get the co-ordinates of these cells and filter out the duplicates.
    -}
    let unrevealed = findAllUnrevealed m
    fs <- filterM isFlagSquare unrevealed
    return $ nub $ map (\c -> (c ^. xpos, c ^. ypos)) fs

-- Determine whether the current cell contains a mine
isFlagSquare :: Cell -> GameState Bool
isFlagSquare c = do
    m <- get
    {-
        Determine whether a cell contains a mine by finding all adjacent cells
        which are revealed. If any of these neighbour cell's number of adjacent
        mines is equal to the number of hidden neighbours then the current cell
        contains a mine.
    -}
    let nc = getNeighbourCords (c ^. xpos) (c ^. ypos)
    let rn = filter (\(x,y) ->
                let result = m ^? board . cells . element x . element y . revealed
                in case result of
                    Nothing -> False
                    _ -> fromJust result) nc
    fc <- mapM adjEqualUnrevealed rn
    return $ True `elem` fc

-- Determine whether the number of hidden adjacent tiles equals the number mines
adjEqualUnrevealed :: (Int, Int) -> GameState Bool
adjEqualUnrevealed (a, b) = do
    m <- get
    -- find the number of unrevealed neigbours and compare with adjacent bombs
    let nc = getNeighbourCords a b
    let adj = fromJust $ m ^? board . cells . element a . element b . adjacentMines
    let rn = filter (\(x,y) ->
            let result = m ^? board . cells . element x . element y . revealed
            in case result of
                Nothing -> False
                _ -> not $ fromJust result) nc
    return $ adj == length rn

-- Function to get the cordinates of all tiles adjacent to a particular location
getNeighbourCords :: Int -> Int -> [(Int, Int)]
getNeighbourCords x y = delete (x,y) $ liftM2 (,) xranges yranges
    where
        xranges = [(x-1)..(x+1)]
        yranges = [(y-1)..(y+1)]
