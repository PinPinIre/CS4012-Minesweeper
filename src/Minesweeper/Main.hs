module Main where

import Minesweeper.Board
import Minesweeper.Cell
import Minesweeper.Game
import Minesweeper.Solver

import Control.Lens hiding (set)
import Control.Monad.State
import qualified Control.Monad.State as State
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

    genBoard p l g

    set f [ layout := margin 5 $ column 5 [floatTop $ widget p, floatLeft $ widget l] ]

genBoard :: Panel () -> StaticText () -> Var Minesweeper -> IO ()
genBoard p st g = do
    b <- varGet g

    let boardCells = b ^. board . cells
        cellsList = Vector.toList $ Vector.map Vector.toList boardCells

    mapM_ (mapM (genButtton p st g)) cellsList

genButtton :: Panel () -> StaticText () -> Var Minesweeper -> Cell -> IO (Button ())
genButtton p st g c = do
    let x = _xpos c
        y = _ypos c

    b <- button p [ text := " "
                  , position := pt (x * buttonHeight) (y * buttonWidth)
                  , size := sz buttonWidth buttonHeight ]

    set b [ on click := reveal x y g b
          , on clickRight := flag x y st g b]

    return b

reveal :: Int -> Int -> Var Minesweeper -> Button () -> Point -> IO ()
reveal x y game b _ = do
    g <- varGet game
    newState <- execStateT (revealGame x y b) g
    varSet game newState

revealGame :: Int -> Int -> Button () -> Game ()
revealGame x y b = do
    status <- revealCell x y

    case status of
        Lose -> do
            liftIO $ set b [ text  := "💣" ]
            fr <- liftIO $ frame [ text := "Minesweeper!" ]
            l <- liftIO $ staticText fr [ text := "You Lose!"]
            liftIO $ set fr [ layout := margin 100 $ floatCenter $ widget l ]
        _ -> do
            adj <- getAdjacentMines x y
            liftIO $ set b [ text := show adj ]

    newState <- State.get
    liftIO $ print newState

flag :: Int -> Int -> StaticText () -> Var Minesweeper -> Button () -> Point -> IO ()
flag x y st game b _ = do
    g <- varGet game
    newState <- execStateT (flagGame x y st b) g
    varSet game newState

flagGame :: Int -> Int -> StaticText () -> Button () -> Game ()
flagGame x y st b = do
    r <- isRevealed x y
    f <- isFlagged x y

    case (r, f) of
        (False, False) -> do
            status <- flagCell x y
            case status of
                OutOfFlags -> do
                    numFlags <- use remainingFlags
                    liftIO $ set st [ text := ("Remaining Flags: " ++ show numFlags) ]
                    liftIO $ set b [ text := "🚩" ]

                    newState <- State.get
                    liftIO $ print newState
                Won -> liftIO $ do
                    fr <- liftIO $ frame [ text := "Minesweeper!"
                                         , bgcolor := white]
                    l <- liftIO $ staticText fr [ text := "You won!"]
                    liftIO $ set fr [ layout := widget l ]
                _ -> do
                    adj <- getAdjacentMines x y
                    liftIO $ set b [ text := show adj ]

        (False, True) -> do
            unflagCell x y
            numFlags <- use remainingFlags
            liftIO $ set st [ text := ("Remaining Flags: " ++ show numFlags) ]
            liftIO $ set b [ text  := " " ]

            newState <- State.get
            liftIO $ print newState
        _ -> return ()