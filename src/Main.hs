module Main where

import Game
import Cell
import Control.Monad.State
import Control.Monad (liftM2)
import Graphics.UI.WX
import Graphics.UI.WXCore
import Data.List (groupBy)
import qualified Data.Function as DF (on)

button_width = 30
button_height = 30

main :: IO ()
main = do
    let minesweeper = initMinesweeper
    let finished = execState testRun minesweeper
    print finished
    start gui

testRun :: State Minesweeper Status
testRun = do
    revealCell (1, 2)
    revealCell (2, 7)
    flagCell (5, 8)

gui :: IO ()
gui = do
    game    <- varCreate $ initMinesweeper

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
genButtton f game c = do
    state <- varGet game
    let
        (status, newstate) = runState (getCellField mined c) state
        (x, y) = c
        xpos = x * button_height
        ypos = y * button_width
    varSet game newstate
    b <- button f [ text  := ""
                  , position := pt ypos xpos
                  , size := sz button_width button_height]
    Graphics.UI.WX.set b [ on click  := reveal f c game b
                         , on clickRight := flag f c game b]
    return b

reveal f c game b _ = do
    state <- varGet game
    let
        (status, newstate) = runState (setRevealed c) state
    Graphics.UI.WX.set b [ text  := "R" ]
    print newstate
    varSet game newstate

flag f c game b _ = do
    state <- varGet game
    let
        (status, newstate) = runState (setFlagged c) state
    Graphics.UI.WX.set b [ text  := "F" ]
    print newstate
    varSet game newstate


layoutBoard :: [[(Button ())]] -> Layout
layoutBoard b = grid 0 0 $ map layoutRow b

layoutRow :: [(Button ())] -> [Layout]
layoutRow r = [row 0 $ map widget r]

groupRows = groupBy ((==) `DF.on` fst)
