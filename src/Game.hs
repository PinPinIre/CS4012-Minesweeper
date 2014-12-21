{-# LANGUAGE TemplateHaskell, RankNTypes #-}

module Game where

import Board
import Cell
import Control.Lens
import Control.Monad.State
import Data.Maybe
import qualified Data.Vector as Vector (concat, toList)

data Minesweeper = Minesweeper
    { _board :: Board
    , _remainingFlags :: Int
    }

makeLenses ''Minesweeper

instance Show Minesweeper where
    show = show . view board

type Pos = (Int, Int)

type Game = State Minesweeper

data Status = Won | Lose | Move deriving (Show, Eq)

initMinesweeper :: Minesweeper
initMinesweeper = Minesweeper { _board = initBoard 20 20
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

revealCell :: Pos -> Game Status
revealCell p = do
    r <- isRevealed p
    if not r then do
        m <- isMined p
        if m then
            return Lose
        else do
            setRevealed p
            won <- isWon

            if won then
                return Won
            else
                return Move
    else
        return Move

flagCell :: Pos -> Game Status
flagCell p = do
    m <- get
    if m ^. remainingFlags > 0 then do
        f <- isFlagged p
        if not f then do
            setFlagged p
            won <- isWon

            if won then
                return Won
            else
                return Move
        else
            return Move
    else
        return Move

isMined :: Pos -> Game Bool
isMined = getCellField mined

isFlagged :: Pos -> Game Bool
isFlagged = getCellField flagged

setFlagged :: Pos -> Game ()
setFlagged = setCellField flagged True

isRevealed :: Pos -> Game Bool
isRevealed = getCellField revealed

setRevealed :: Pos -> Game ()
setRevealed = setCellField revealed True

getAdjacentMines :: Pos -> Game Int
getAdjacentMines = getCellField adjacentMines

getCellField :: Getter Cell a -> Pos -> Game a
getCellField getter (x, y) = do
    m <- get
    return $ fromJust $ m ^? (board . cells . element y . element x . getter)

setCellField :: Setter Cell Cell a b -> b -> Pos -> Game ()
setCellField setter val (x, y) = modify $ combinedSetter .~ val
    where combinedSetter = board . cells . element x . element y . setter