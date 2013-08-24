module Main where

import Application.Boot

import World.Geometry
import qualified World.World as W
import qualified World.Player as P
import qualified World.Items.Karyon as K
import qualified World.Items.Stone as S
import qualified World.Items.Canyon as C

import System.Random

world = W.worldFromList (
       K.karyon 1 P.player1 100 (point 10 5 0)
    ++ K.karyon 2 P.player1 100 (point (-10) 5 0)
    ++ S.stone 3 P.stonePlayer (point 1 2 0)
    ++ S.stone 4 P.stonePlayer (point 1 3 0)
    ++ S.stone 5 P.stonePlayer (point 1 4 0)
    ++ S.stone 6 P.stonePlayer (point 1 5 0)
    ++ C.canyon 7 P.canyonPlayer (point 2 2 0)
    ++ C.canyon 8 P.canyonPlayer (point 2 3 0)
    ++ C.canyon 9 P.canyonPlayer (point 2 4 0)
    ++ C.canyon 10 P.canyonPlayer (point 2 5 0))


main::IO ()
main = do

    putStrLn "Loading..."
    
    --boot
    
    let g = mkStdGen 100
    let (_, wm) = W.activateWorld g world
    putStrLn $ "Old StdGen: " ++ show g
    putStrLn $ "New StdGen: " ++ show (W.worldMutatorRndGen wm)
    putStrLn $ "Actions count: " ++ (show . length $ W.worldMutatorActions wm)
    putStrLn $ unlines (map W.showAction (W.worldMutatorActions wm))
    
    putStrLn "All Ok."