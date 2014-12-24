module Minesweeper.Solver where

import Minesweeper.Board
import Minesweeper.Cell
import Minesweeper.Game

import Control.Lens
import Control.Monad.State
import Data.List (delete, nub)
import Data.Maybe
import qualified Data.Vector as Vector

findAllRevealed :: Game -> [Cell]
findAllRevealed b = revealedCells
    where
        boardCells = b ^. board . cells
        filteredCells = Vector.map (Vector.toList . Vector.filter _revealed) boardCells
        revealedCells = concat $ Vector.toList filteredCells

findAllUnrevealed :: Game -> [Cell]
findAllUnrevealed b = revealedCells
    where
        boardCells = b ^. board . cells
        revealedCells = concat $ Vector.toList $ Vector.map (Vector.toList . Vector.filter (not . _revealed)) boardCells

filterZeroAdj ::  [Cell] -> [Cell]
filterZeroAdj = filter (\x -> _adjacentMines x == 0)

findSafeSquares :: GameState [(Int, Int)]
findSafeSquares = do
    m <- get
    let zeroCells = filterZeroAdj $ findAllRevealed m
    ss <- mapM getSafeSquares zeroCells
    return $ nub $ concat ss

getSafeSquares :: Cell -> GameState [(Int, Int)]
getSafeSquares c = do
    m <- get
    let nc = getNeighbourCords (c ^. xpos) (c ^. ypos)
    filterM (\(x,y) -> do
        let result = m ^? board . cells . element x . element y . revealed
        case result of
            Nothing -> return False
            _ -> return (not $ fromJust result)) nc

findFlagSquares :: GameState [(Int, Int)]
findFlagSquares = do
    m <- get
    let unrevealed = findAllUnrevealed m
    fs <- filterM isFlagSquare unrevealed
    return $ nub $ map (\c -> (c ^. xpos, c ^. ypos)) fs

isFlagSquare :: Cell -> GameState Bool
isFlagSquare c = do
    m <- get
    let nc = getNeighbourCords (c ^. xpos) (c ^. ypos)
    let rn = filter (\(x,y) -> do
                let result = m ^? board . cells . element x . element y . revealed
                case result of
                    Nothing -> False
                    _ -> fromJust result) nc
    fc <- mapM adjEqualUnrevealed rn
    return $ True `elem` fc

adjEqualUnrevealed :: (Int, Int) -> GameState Bool
adjEqualUnrevealed (a, b) = do
    m <- get

    let nc = getNeighbourCords a b
    let adj = fromJust $ m ^? board . cells . element a . element b . adjacentMines

    let rn = filter (\(x,y) ->
            let result = m ^? board . cells . element x . element y . revealed
            in case result of
                Nothing -> False
                _ -> not $ fromJust result) nc
    return $ adj == length rn

getNeighbourCords :: Int -> Int -> [(Int, Int)]
getNeighbourCords x y = delete (x,y) $ liftM2 (,) xranges yranges
    where
        xranges = [(x-1)..(x+1)]
        yranges = [(y-1)..(y+1)]
