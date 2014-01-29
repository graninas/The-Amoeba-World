module Main where

import Middleware.Config.Facade

import Application.Boot

optionsFile = "./Data/Options.cfg"

main::IO ()
main = do

    cfg <- loadConfiguration optionsFile
    putStrLn $ getOption cfg appName
    
    boot cfg
    
    putStrLn "All Ok."