module Main where

import Board
import Cell
import Control.Monad.State
import Game
import Graphics.UI.WX
import qualified Data.Vector as Vector
import System.Random

buttonWidth :: Int
buttonWidth = 30

buttonHeight :: Int
buttonHeight = 30

main :: IO ()
main = start gui

gui :: IO ()
gui = do
    rng <- newStdGen

    let minesweeper = initMinesweeper rng
    g <- varCreate minesweeper

    f <- frame [ text := "Minesweeper!"
               , bgcolor := white]

    let numFlags = _remainingFlags minesweeper
    l <- staticText f [ text := ("Remaining Flags: " ++ show numFlags) ]

    p <- panel f []

    _ <- genBoard p l g

    set f [ layout := margin 5 $ column 5 [floatTop $ widget p, floatLeft $ widget l] ]

genBoard :: Panel () -> StaticText () -> Var Minesweeper -> IO [[Button ()]]
genBoard p st g = do
    b <- varGet g

    let boardCells = _cells $ _board b
        cellsList = Vector.toList $ Vector.map Vector.toList boardCells

    mapM (mapM (genButtton p st g)) cellsList

genButtton :: Panel () -> StaticText () -> Var Minesweeper -> Cell -> IO (Button ())
genButtton p st g c = do
    gameState <- varGet g

    let _ = runState (getCellField mined x y) gameState
        x = _xpos c
        y = _ypos c

    b <- button p [ text := " "
                  , position := pt (x * buttonHeight) (y * buttonWidth)
                  , size := sz buttonWidth buttonHeight ]

    set b [ on click := reveal x y g b
          , on clickRight := flag x y st g b]

    return b

reveal :: Int -> Int -> Var Minesweeper -> Button () -> Point -> IO ()
reveal x y game b _ = do
    gameState <- varGet game

    let (revealedState, _)  = runState (getCellField revealed x y) gameState
        (minedState, _)  = runState (getCellField mined x y) gameState
        (_, newState) = runState (setRevealed x y) gameState

    case (revealedState, minedState) of
        (False, False) -> do
            let (adj, _) = runState (getAdjacentMines x y) gameState
            set b [ text := show adj ]
            print newState
            varSet game newState
        (False, _) -> do
            set b [ text  := "💣" ]
            print newState
            varSet game newState
        _ -> return ()

flag :: Int -> Int -> StaticText () -> Var Minesweeper -> Button () -> Point -> IO ()
flag x y st game b _ = do
    gameState <- varGet game

    let (revealedState, _) = runState (isRevealed x y) gameState
        (flaggedState, _) = runState (isFlagged x y) gameState

    case (revealedState, flaggedState) of
        (False, False) -> do
            let (flagStatus, newState) = runState (flagCell x y) gameState
                numFlags = _remainingFlags newState

            unless (flagStatus == OutOfFlags) $ do
                set st [ text := ("Remaining Flags: " ++ show numFlags) ]

                set b [ text := "🚩" ]
                print newState
                varSet game newState

        (False, True) -> do
            let (_, newState) = runState (unflagCell x y) gameState
            set b [ text  := "" ]
            print newState
            varSet game newState
        _ -> return ()

layoutBoard :: [[Button ()]] -> Layout
layoutBoard b = grid 0 0 $ map layoutRow b

layoutRow :: [Button ()] -> [Layout]
layoutRow r = [row 0 $ map widget r]
