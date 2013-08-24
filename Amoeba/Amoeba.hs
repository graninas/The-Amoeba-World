module Main where

import Application.Boot

import qualified World.World as W
import qualified World.Karyon as K
import qualified World.Player as P
import World.Geometry

world = W.worldMapFromList (K.karyon P.player1 100 (point 10 5 0))


main::IO ()
main = do

    putStrLn "Loading..."
    
    boot
    
    putStrLn $ "All Ok."