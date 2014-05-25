module Amoeba where

import Middleware.Config.Facade
import Application.Boot
import Paths_The_Amoeba_World as P

optionsFile = "./Game/Data/Options.cfg"

run::IO ()
run = do

    realOptionsFileName <- P.getDataFileName optionsFile
    cfg <- loadConfiguration realOptionsFileName
    boot cfg
    
    putStrLn "All Ok."