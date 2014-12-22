module Main where

import Board
import Cell
import Control.Lens
import Control.Monad.State
import Game
import Graphics.UI.WX
import qualified Graphics.UI.WX as WX
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

    g <- varCreate $ initMinesweeper rng

    f <- frame [ text := "Minesweeper!"
               , bgcolor := white]

    p <- panel f []

    _ <- genBoard p g

    WX.set f [layout := widget p]

genBoard :: Panel () -> Var Minesweeper -> IO [[Button ()]]
genBoard f g = do
    b <- varGet g

    let boardCells = b ^. board . cells
        cellsList = Vector.toList $ Vector.map Vector.toList boardCells

    mapM (mapM (genButtton f g)) cellsList

genButtton :: Panel () -> Var Minesweeper -> Cell -> IO (Button ())
genButtton f g c = do
    gameState <- varGet g

    let _ = runState (getCellField mined x y) gameState

        x = c ^. xpos
        y = c ^. ypos

    b <- button f [ text := " "
                  , position := pt (x * buttonHeight) (y * buttonWidth)
                  , size := sz buttonWidth buttonHeight]

    WX.set b [ on click := reveal x y g b
             , on clickRight := flag x y g b]

    return b

reveal :: Int -> Int -> Var Minesweeper -> Button () -> Point -> IO ()
reveal x y game b _ = do
    gameState <- varGet game

    let (revealedState, _)  = runState (getCellField revealed x y) gameState
        (minedState, _)  = runState (getCellField mined x y) gameState
        (_, newstate) = runState (setRevealed x y) gameState

    case (revealedState, minedState) of
        (False, False) -> do
            let (adj, _) = runState (getAdjacentMines x y) gameState
            WX.set b [ text := show adj ]
            print newstate
            varSet game newstate
        (False, _) -> do
            WX.set b [ text  := "💣" ]
            print newstate
            varSet game newstate
        _ -> return ()

flag :: Int -> Int -> Var Minesweeper -> Button () -> Point -> IO ()
flag x y game b _ = do
    gameState <- varGet game
    let
        (revealedState, _) = runState (getCellField revealed x y) gameState
        (_, newstate) = runState (setFlagged x y) gameState

    unless revealedState $ do
        WX.set b [ text  := "🚩" ]
        print newstate
        varSet game newstate

layoutBoard :: [[Button ()]] -> Layout
layoutBoard b = grid 0 0 $ map layoutRow b

layoutRow :: [Button ()] -> [Layout]
layoutRow r = [row 0 $ map widget r]
