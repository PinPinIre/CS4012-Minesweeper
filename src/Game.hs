{-# LANGUAGE TemplateHaskell, RankNTypes #-}

module Game where

import Board
import Cell
import Control.Lens
import Control.Monad.State
import Data.Maybe
import qualified Data.Vector as Vector (concat, toList)

type Pos = (Int, Int)

data Status = Won | Lose | Move deriving (Show, Eq)

data Minesweeper = Minesweeper
    { _board :: Board
    , _remainingFlags :: Int
    }

makeLenses ''Minesweeper

instance Show Minesweeper where
    show = show . view board

type Game = State Minesweeper

initMinesweeper :: Minesweeper
initMinesweeper = Minesweeper { _board = initBoard 20 20
                              , _remainingFlags = 10
                              }

checkStatus :: Game Status
checkStatus = do
    m <- get
    let boardCells = m ^. board . cells
    let concatCells = Vector.toList $ Vector.concat $ Vector.toList boardCells
    let cellStates = map checkCellStatus concatCells

    if all (==Won) cellStates then
        return Won
    else if Lose `elem` cellStates then
        return Lose
    else
        return Move

checkCellStatus :: Cell -> Status
checkCellStatus c
    | c ^. flagged && c ^. mined = Won
    | c ^. revealed && c ^. mined = Lose
    | otherwise = Move


revealCell :: Pos -> Game Status
revealCell p = do
    r <- isRevealed p
    if not r then do
        m <- isMined p
        if m then
            return Lose
        else
            return Move
    else
        return Move

isMined :: Pos -> Game Bool
isMined = getCellField mined

isFlagged :: Pos -> Game Bool
isFlagged = getCellField flagged

isRevealed :: Pos -> Game Bool
isRevealed = getCellField revealed

getAdjacentMines :: Pos -> Game Int
getAdjacentMines = getCellField adjacentMines

getCellField :: Getter Cell a -> Pos -> Game a
getCellField getter (x, y) = do
    m <- get
    return $ fromJust $ m ^? (board . cells . element y . element x . getter)
