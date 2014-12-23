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

    let numFlags = minesweeper ^. remainingFlags
    l <- staticText f [ text := ("Remaining Flags: " ++ show numFlags) ]
    p <- panel f []
    p2 <- panel f []

    bs <- genBoard p l g
    b <- button p2 [ text := "Solve"
                  , on click := solve g bs]
    set f [ layout := margin 5 $ column 5 [floatTop $ widget p, floatLeft $ widget l, floatRight $ widget p2] ]

genBoard :: Panel () -> StaticText () -> Var Minesweeper -> IO ([[Button ()]])
genBoard p st g = do
    b <- varGet g

    let boardCells = b ^. board . cells
        cellsList = Vector.toList $ Vector.map Vector.toList boardCells

    mapM (mapM (genButtton p st g)) cellsList

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
                Move -> do
                    numFlags <- use remainingFlags
                    liftIO $ set st [ text := ("Remaining Flags: " ++ show numFlags) ]
                    liftIO $ set b [ text := "🚩" ]

                    newState <- State.get
                    liftIO $ print newState

                OutOfFlags -> return ()

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

solve :: Var Minesweeper -> [[Button ()]] -> Point -> IO ()
solve game bs _ = do
    g <- varGet game
    let (safeMoves, _) = runState findSafeSquares g
    liftIO $ print safeMoves
    revealList game bs safeMoves
    return ()

revealList :: Var Minesweeper -> [[Button ()]] -> [(Int, Int)] -> IO ()
revealList game bs cs = do
    mapM (\(x, y) -> do
            let b = ((bs)!!y)!!x
            reveal y x game b (pt 0 0)) cs
    return ()
