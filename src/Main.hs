module Main where

import Amoeba.Application.Boot
import Amoeba.Middleware.Config.Facade
import Paths_The_Amoeba_World as P

optionsFile = "./Game/Data/Options.cfg"

main :: IO ()
main = do

    realOptionsFileName <- P.getDataFileName optionsFile
    cfg <- loadConfiguration realOptionsFileName
    boot cfg
    
    putStrLn "All Ok."