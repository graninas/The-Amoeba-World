module Amoeba where

import Middleware.Config.Facade

import Application.Boot

optionsFile = "./Data/Options.cfg"

run::IO ()
run = do

    cfg <- loadConfiguration optionsFile
    boot cfg
    
    putStrLn "All Ok."