module Main where

import Game
import Cell
import Control.Monad.State
import Control.Monad (liftM2)
import Graphics.UI.WX
import Graphics.UI.WXCore
import Data.List (groupBy)
import System.Random
import qualified Data.Function as DF (on)

button_width = 30
button_height = 30

main :: IO ()
main = do
    rng <- newStdGen
    let minesweeper = initMinesweeper rng
    let finished = execState testRun minesweeper
    print finished
    start gui

testRun :: State Minesweeper Status
testRun = do
    _ <- revealCell 1 2
    _ <- revealCell 2 7
    flagCell 5 8

gui :: IO ()
gui = do
    rng <- newStdGen
    game    <- varCreate $ initMinesweeper rng

    let
        cells = groupRows $ liftM2 (,) [0..19] [0..19]

    f   <- frame [text          := "Minesweeper!"
                 , bgcolor      := white ]
    p   <- panel f []
    status <- statusField [text := "Welcome to Minesweeper"]
    bb <- genBoard p game cells
    let
        bblayout = layoutBoard bb
    Graphics.UI.WX.set f [ statusBar := [status]
                         , layout := widget p ]

genBoard :: Panel() -> Var Minesweeper -> [[(Int, Int)]] -> IO [[(Button ())]]
genBoard f game cells  = mapM (genRow f game) cells

genRow :: Panel() -> Var Minesweeper -> [(Int, Int)] -> IO [(Button ())]
genRow f game cells =  mapM (genButtton f game) cells

genButtton :: Panel() -> Var Minesweeper -> (Int, Int) -> IO (Button ())
genButtton f game (x, y) = do
    state <- varGet game
    let
        (status, _) = runState (getCellField mined x y) state
        xpos = x * button_height
        ypos = y * button_width
    b <- button f [ text  := ""
                  , position := pt xpos ypos
                  , size := sz button_width button_height]
    Graphics.UI.WX.set b [ on click  := reveal f x y game b
                         , on clickRight := flag f x y game b]
    return b

reveal f x y game b _ = do
    state <- varGet game
    let
        (flipped, _) = runState (getCellField revealed x y) state
        (ismined, _) = runState (getCellField mined x y) state
        (_, newstate) = runState (setRevealed x y) state
    case (flipped, ismined) of
        (False, False) -> do
            let
                (adj, _) = runState (getAdjacentMines x y) state
            Graphics.UI.WX.set b [ text  := show adj ]
            print newstate
            varSet game newstate
        (False, _) -> do
            Graphics.UI.WX.set b [ text  := "💣" ]
            print newstate
            varSet game newstate
        otherwise -> do return()

flag f x y game b _ = do
    state <- varGet game
    let
        (flipped, _) = runState (getCellField revealed x y) state
        (_, newstate) = runState (setFlagged x y) state
    case flipped of
        False -> do
            Graphics.UI.WX.set b [ text  := "⚑" ]
            print newstate
            varSet game newstate
        otherwise -> do return()


layoutBoard :: [[(Button ())]] -> Layout
layoutBoard b = grid 0 0 $ map layoutRow b

layoutRow :: [(Button ())] -> [Layout]
layoutRow r = [row 0 $ map widget r]

groupRows = groupBy ((==) `DF.on` fst)
