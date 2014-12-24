{-# LANGUAGE TemplateHaskell, RankNTypes #-}

module Minesweeper.Game where

import Minesweeper.Board
import Minesweeper.Cell

import Control.Lens
import Control.Monad.State
import Data.Maybe
import qualified Data.Vector as Vector (concat, toList)
import System.Random

data Game = Game
    { _board :: Board
    , _remainingFlags :: Int
    }

makeLenses ''Game

instance Show Game where
    show = show . view board

type GameState = StateT Game IO

data Status = Won | Lose | Error | Move deriving (Show, Eq)

initGame :: StdGen -> Game
initGame rng = Game { _board = initBoard 20 20 10 rng
                    , _remainingFlags = 10
                    }

isWon :: GameState Bool
isWon = do
    boardCells <- use $ board . cells
    let concatCells = Vector.toList $ Vector.concat $ Vector.toList boardCells
    let cellStates = map checkCellStatus concatCells

    if all (==True) cellStates then
        return True
    else
        return False

checkCellStatus :: Cell -> Bool
checkCellStatus c
    | c ^. flagged && c ^. mined = True
    | not (c ^. mined)           = True
    | otherwise                  = False

toggleRevealCell :: Int -> Int -> GameState Status
toggleRevealCell x y = do
    r <- isRevealed x y
    f <- isFlagged x y

    if not r && not f then do
        m <- isMined x y
        if m then
            return Lose
        else do
            setCellField revealed True x y

            won <- isWon
            if won then
                return Won
            else
                return Move
    else
        return Error

toggleFlagCell :: Int -> Int -> GameState Status
toggleFlagCell x y = do
    r <- isRevealed x y

    if not r then do
        f <- isFlagged x y

        if not f then do
            numFlags <- use remainingFlags

            if numFlags > 0 then do
                setCellField flagged True x y
                remainingFlags -= 1

                won <- isWon
                if won then
                    return Won
                else
                    return Move
            else
                return Error
        else do
            setCellField flagged False x y
            remainingFlags += 1

            return Move
    else
        return Error

isMined :: Int -> Int -> GameState Bool
isMined = getCellField mined

isFlagged :: Int -> Int -> GameState Bool
isFlagged = getCellField flagged

isRevealed :: Int -> Int -> GameState Bool
isRevealed = getCellField revealed

getAdjacentMines :: Int -> Int -> GameState Int
getAdjacentMines = getCellField adjacentMines

getCellField :: Getter Cell a -> Int -> Int -> GameState a
getCellField getter x y = do
    m <- get
    return $ fromJust $ m ^? board . cells . element y . element x . getter

setCellField :: Setter Cell Cell a b -> b -> Int -> Int -> GameState ()
setCellField setter val x y = combinedSetter .= val
    where combinedSetter = board . cells . element y . element x . setter
