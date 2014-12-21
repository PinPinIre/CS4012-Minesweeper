module Main where

import Game
import Control.Monad.State
import Graphics.UI.WX
import Graphics.UI.WXCore
import System.Random
import Control.Lens

height   :: Int
height   = 300

width    :: Int
width    = 300

main :: IO ()
main = do
    let minesweeper = initMinesweeper
    let finished = execState testRun minesweeper
    print finished

testRun :: State Minesweeper Status
testRun = do
    revealCell (1, 2)
    revealCell (2, 7)
    flagCell (5, 8)

gui :: IO ()
gui = do
    g    <- getStdGen
    game <- varCreate $ initMinesweeper
    board<- varGet game
    let
        button_width = 50
        button_height = 50
        cells = gameState board

    f     <- frame    [text          := "Minesweeper!"
                      , bgcolor      := white
                      , resizeable   := False]
    status <- statusField [text := "Welcome to Minesweeper"]
    Graphics.UI.WX.set f [statusBar := [status]]
    pp <- panel f []
    bb <- genBoard f cells
    let
        bblayout = layoutBoard bb
    Graphics.UI.WX.set f [ layout          := bblayout
                         , bgcolor         := white ]

genBoard :: Frame() -> [[Cell]] -> IO [[(Button ())]]
genBoard f cells = mapM (genRow f) cells

genRow :: Frame() -> [Cell] -> IO [(Button ())]
genRow f cells =  mapM (genButtton f) cells

genButtton :: Frame() -> Cell -> IO (Button ())
genButtton f c
                | (c ^. revealed) && (c ^. adjacentMines == 0) = do
                    button f [ text  := show c]
                | c ^. flagged = do
                    button f [ text  := show c]
                | otherwise = do
                    button f [ text  := " "]

layoutBoard :: [[(Button ())]] -> Layout
layoutBoard b = grid 0 0 $ map layoutRow b

layoutRow :: [(Button ())] -> [Layout]
layoutRow r = [row 0 $ map widget r]

addButtton :: Frame() -> [Char] -> Int -> Int -> Int -> Int -> IO (Button ())
addButtton parent string x y button_width button_height = do
        let
            xpos = x * button_height
            ypos = y * button_width
        button parent [ text  := string
                      , position := pt ypos xpos
                      , size  := sz button_height button_width]
