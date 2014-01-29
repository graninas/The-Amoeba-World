module Main where

import Middleware.Config.Facade

import Application.Boot

optionsFile = "./Data/Options.cfg"

main::IO ()
main = do

    cfg <- loadConfiguration optionsFile
    putStrLn $ getConfig cfg appName
    putStrLn $ getConfig cfg screenWidth
    putStrLn $ getConfig cfg screenHeight
    
    putStrLn "All Ok."