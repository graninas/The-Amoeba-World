module Main where

import Application.Boot

import qualified World.World as W
import qualified World.Karyon as K
import qualified World.Player as P
import World.Geometry

import System.Random

world = W.worldFromList (K.karyon 1 P.player1 100 (point 10 5 0))


main::IO ()
main = do

    putStrLn "Loading..."
    
    --boot
    
    let g = mkStdGen 100
    let (_, wm) = W.activateWorld g world
    putStrLn $ "Old StdGen: " ++ show g
    putStrLn $ "New StdGen: " ++ show (W.worldMutatorRndGen wm)
    putStrLn $ "Actions count: " ++ (show . length $ W.worldMutatorActions wm)
    
    putStrLn "All Ok."