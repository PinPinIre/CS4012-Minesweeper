{-# LANGUAGE TemplateHaskell, RankNTypes #-}

module Game where

import Board
import Cell
import Control.Lens
import Control.Monad.State
import Data.Maybe
import qualified Data.Vector as Vector (concat, toList)
import System.Random

data Minesweeper = Minesweeper
    { _board :: Board
    , _remainingFlags :: Int
    }

makeLenses ''Minesweeper

instance Show Minesweeper where
    show = show . view board

type Game = State Minesweeper

data Status = Won | Lose | Move deriving (Show, Eq)

initMinesweeper :: StdGen -> Minesweeper
initMinesweeper rng = Minesweeper { _board = initBoard 20 20 10 rng
                                  , _remainingFlags = 10
                                  }

isWon :: Game Bool
isWon = do
    m <- get

    let boardCells = m ^. board . cells
    let concatCells = Vector.toList $ Vector.concat $ Vector.toList boardCells
    let cellStates = map checkCellStatus concatCells

    if all (==True) cellStates then
        return True
    else
        return False

checkCellStatus :: Cell -> Bool
checkCellStatus c
    | c ^. flagged && c ^. mined = True
    | otherwise                  = False

revealCell :: Int -> Int -> Game Status
revealCell x y = do
    r <- isRevealed x y
    if not r then do
        m <- isMined x y
        if m then
            return Lose
        else do
            setRevealed x y
            won <- isWon

            if won then
                return Won
            else
                return Move
    else
        return Move

flagCell :: Int -> Int -> Game Status
flagCell x y = do
    numFlags <- use remainingFlags
    if numFlags > 0 then do
        f <- isFlagged x y
        if not f then do
            setCellField flagged True x y
            remainingFlags -= 1

            won <- isWon
            if won then
                return Won
            else
                return Move
        else
            return Move
    else
        return Move

unflagCell :: Int -> Int -> Game ()
unflagCell x y = do
    setCellField flagged False x y
    remainingFlags += 1

isMined :: Int -> Int -> Game Bool
isMined = getCellField mined

isFlagged :: Int -> Int -> Game Bool
isFlagged = getCellField flagged

isRevealed :: Int -> Int -> Game Bool
isRevealed = getCellField revealed

setRevealed :: Int -> Int -> Game ()
setRevealed = setCellField revealed True

getAdjacentMines :: Int -> Int -> Game Int
getAdjacentMines = getCellField adjacentMines

getCellField :: Getter Cell a -> Int -> Int -> Game a
getCellField getter x y = do
    m <- get
    return $ fromJust $ m ^? board . cells . element y . element x . getter

setCellField :: Setter Cell Cell a b -> b -> Int -> Int -> Game ()
setCellField setter val x y = combinedSetter .= val
    where combinedSetter = board . cells . element y . element x . setter
