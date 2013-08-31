module Main where

import Application.Boot

import World.Geometry
import World.World
import qualified World.Player as P
import qualified World.Items.Karyon as K
import qualified World.Items.Stone as S
import qualified World.Items.Canyon as C
import qualified World.Items.Border as B

import System.Random

itemsList :: [(Point, ActiveItem)]
itemsList =      K.karyon 1 P.player1 100 (point 10 5 0)
            |>|  K.karyon 2 P.player2 100 (point (-10) 5 0)
            |>|  S.stone 3 P.stonePlayer (point 1 2 0)
            |>|  S.stone 4 P.stonePlayer (point 1 3 0)
            |>|  S.stone 5 P.stonePlayer (point 1 4 0)
            |>|  S.stone 6 P.stonePlayer (point 1 5 0)
            |>|  C.canyon 7 P.canyonPlayer (point 2 2 0)
            |>|  C.canyon 8 P.canyonPlayer (point 2 3 0)
            |>|  C.canyon 9 P.canyonPlayer (point 2 4 0)
            |>|  C.canyon 10 P.canyonPlayer (point 2 5 0)
            |>|| B.border 11 P.player1 (point 1 1 1)

world = newWorld (worldMapFromList itemsList) 10

step w currentMove = do
    putStrLn $ "\n=========" ++ show currentMove ++ "========="
    let res@(w', anns) = stepWorld w
    putStrLn $ "Items count: " ++ show (itemsCount w')
    mapM_ (putStrLn . annotationMessage) anns
    return res

eval w 0 = return ()
eval w n = do
    let currentMove = movesCount - n
    (w', anns) <- step w currentMove
    appendFile gameLogFile ("\n=========" ++ show currentMove ++ "=========")
    appendFile gameLogFile (unlines . map annotationMessage $ anns)
    eval w' (n-1)

gameLogFile = "moves.txt"
movesCount = 3

main::IO ()
main = do

    putStrLn "Loading..."
    writeFile gameLogFile "Game moves:"
    --boot
    let w = world (mkStdGen 100)
    
    eval w movesCount
    
    putStrLn "All Ok."