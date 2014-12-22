module Main where

import Board
import Cell
import Control.Lens
import Control.Monad.State
import Game
import Graphics.UI.WX
import qualified Graphics.UI.WX as WX
import Data.List (groupBy)
import System.Random
import qualified Data.Function as DF (on)

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
    game <- varCreate minesweeper

    let w = minesweeper ^. board . width - 1
        h = minesweeper ^. board . height - 1
        cellIndexes = groupRows $ liftM2 (,) [0..w] [0..h]

    f <- frame [ text := "Minesweeper!"
               , bgcolor := white]

    p <- panel f []

    status <- statusField [text := "Welcome to Minesweeper"]

    _ <- genBoard p game cellIndexes

    WX.set f [ statusBar := [status]
             , layout := widget p]

genBoard :: Panel () -> Var Minesweeper -> [[(Int, Int)]] -> IO [[Button ()]]
genBoard f game  = mapM (genRow f game)

genRow :: Panel () -> Var Minesweeper -> [(Int, Int)] -> IO [Button ()]
genRow f game =  mapM (genButtton f game)

genButtton :: Panel () -> Var Minesweeper -> (Int, Int) -> IO (Button ())
genButtton f game (x, y) = do
    gameState <- varGet game

    let _ = runState (getCellField mined x y) gameState

    let xButton = x * buttonHeight
    let yButton = y * buttonWidth

    b <- button f [ text := " "
                  , position := pt xButton yButton
                  , size := sz buttonWidth buttonHeight]

    WX.set b [ on click := reveal x y game b
             , on clickRight := flag x y game b]

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

groupRows :: [(Int, a)] -> [[(Int, a)]]
groupRows = groupBy ((==) `DF.on` fst)
