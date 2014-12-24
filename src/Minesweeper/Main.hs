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
import System.Exit
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

    let game = initGame rng
    g <- varCreate game

    f <- frame [ text := "Minesweeper"
               , bgcolor := white]

    let numFlags = game ^. remainingFlags
    l <- staticText f [ text := ("Remaining Flags: " ++ show numFlags) ]
    p <- panel f []

    buttons <- genBoard f p l g

    s <- button f [ text := "Solve"
                  , on click := solve f g buttons l ]

    set f [ layout := margin 5 $ column 5 [ floatTop $ widget p
                                          , row 10 [ floatLeft $ widget l
                                                   , floatRight $ widget s]] ]

genBoard :: Frame () -> Panel () -> StaticText () -> Var Game -> IO [[Button ()]]
genBoard f p st g = do
    b <- varGet g

    let boardCells = b ^. board . cells
        cellsList = Vector.toList $ Vector.map Vector.toList boardCells

    mapM (mapM (genButtton f p st g)) cellsList

genButtton :: Frame () -> Panel () -> StaticText () -> Var Game -> Cell -> IO (Button ())
genButtton f p st g c = do
    let x = _xpos c
        y = _ypos c

    b <- button p [ text := " "
                  , position := pt (x * buttonHeight) (y * buttonWidth)
                  , size := sz buttonWidth buttonHeight ]

    set b [ on click := reveal x y f g b
          , on clickRight := flag x y f st g b]

    return b

reveal :: Int -> Int -> Frame () -> Var Game -> Button () -> Point -> IO ()
reveal x y f game b _ = do
    g <- varGet game
    newState <- execStateT (revealGame x y f b) g
    varSet game newState

revealGame :: Int -> Int -> Frame () -> Button () -> GameState ()
revealGame x y f b = do
    status <- toggleRevealCell x y

    case status of
        Move -> do
            adj <- getAdjacentMines x y
            liftIO $ set b [ text := show adj ]

        Lose -> do
            liftIO $ set b [ text  := "💣" ]
            liftIO $ presentAlert f "You Lost!"

        _ -> return ()

    newState <- State.get
    liftIO $ print newState

flag :: Int -> Int -> Frame () -> StaticText () -> Var Game -> Button () -> Point -> IO ()
flag x y f st game b _ = do
    g <- varGet game
    newState <- execStateT (flagGame x y f st b) g
    varSet game newState

flagGame :: Int -> Int -> Frame () -> StaticText () -> Button () -> GameState ()
flagGame x y f st b = do
    status <- toggleFlagCell x y

    case status of
        Move -> do
            numFlags <- use remainingFlags
            liftIO $ set st [ text := ("Remaining Flags: " ++ show numFlags) ]

            cellFlagged <- isFlagged x y
            if cellFlagged then
                liftIO $ set b [ text := "🚩" ]
            else
                liftIO $ set b [ text := "" ]

        Won -> liftIO $ do
            set b [ text := "🚩" ]
            presentAlert f "You Won!"

        Error -> return ()

        _ -> do
            adj <- getAdjacentMines x y
            liftIO $ set b [ text := show adj ]

    newState <- State.get
    liftIO $ print newState

presentAlert :: Frame () -> String -> IO ()
presentAlert f t = do
    close f

    alertFrame  <- frame [ text := "Minesweeper"]

    l  <- staticText alertFrame [ text := t]

    b1 <- button alertFrame [ text := "Play Again"
                            , size := sz 75 30 ]
    b2 <- button alertFrame [ text := "Quit"
                            , size := sz 75 30 ]

    set b1 [ on click := playAgain alertFrame ]
    set b2 [ on click := quit ]

    set alertFrame [ layout := marginWidth 20 $ marginBottom $ marginTop $
                               marginWidth 150 $ marginLeft $ marginRight $
                               column 20 [floatCenter $ widget l, row 10 [widget b1, widget b2]] ]

playAgain :: Frame () -> Point -> IO ()
playAgain f _ = close f >> gui

quit :: Point -> IO ()
quit _ = exitSuccess

solve :: Frame () -> Var Game -> [[Button ()]] -> StaticText () -> Point -> IO ()
solve f game bs l _ = do
    g <- varGet game
    safeMoves <- evalStateT findSafeSquares g
    case safeMoves of
        []  -> do
            flagMoves <- evalStateT findFlagSquares g
            flagList f game bs l flagMoves

        _   -> revealList f game bs safeMoves

revealList :: Frame () -> Var Game -> [[Button ()]] -> [(Int, Int)] -> IO ()
revealList f game bs = mapM_ (\(x, y) -> do
                        let b = (bs !! y) !! x
                        reveal y x f game b (pt 0 0))

flagList :: Frame () -> Var Game -> [[Button ()]] -> StaticText () -> [(Int, Int)] -> IO ()
flagList f game bs l = mapM_ (\(x, y) -> do
                            let b = (bs !! y) !! x
                            flag y x f l game b (pt 0 0))
