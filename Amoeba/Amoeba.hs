module Main where

import Application.Boot
import World.Geometry
import World.World
import World.Utils
import qualified World.Player as P
import qualified World.Items.Karyon as K
import qualified World.Items.Stone as S
import qualified World.Items.Canyon as C
import qualified World.Items.Border as B

import System.Random

gameLogFile = "moves.txt"
movesCount = 30

itemsList :: [(Point, ActiveItem)]
itemsList =      K.karyon 1 P.player1 100 (point 10 5 0)
            |>|  K.karyon 2 P.player2 100 (point 5 7 0)
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
    --putStrLn $ "\n=========" ++ show currentMove ++ "========="
    let res@(w', anns) = stepWorld w
    --putStrLn $ "Items count: " ++ show (itemsCount w')
    --mapM_ (putStrLn . annotationMessage) anns
    return res

eval w 0 = return ()
eval w n = do
    let currentMove = movesCount - n + 1
    res@(w', anns) <- step w currentMove
    logWorld gameLogFile res currentMove
    eval w' (n-1)

main::IO ()
main = do
    putStrLn "Loading..."
    
    let w = world (mkStdGen 100)
    boot w
    
    --writeFile gameLogFile "Game moves:"
    --logWorld gameLogFile (w,[]) 0
    --eval w movesCount
    
    putStrLn "All Ok."