module Main where

import Amoeba.Application.Boot
import Amoeba.Application.Game.GameDataLoader
import Amoeba.Application.Assets.ConfigScheme as Scheme
import qualified Amoeba.Middleware.Config.Facade as Cfg
import qualified Amoeba.Middleware.Tracing.Log as Log

import Paths_The_Amoeba_World as P

optionsPath = "./Game/Data/Options.cfg"
logFileLoader   = Cfg.filePathLoader Scheme.logPath "Amoeba.log"
worldFileLoader = Cfg.filePathLoader Scheme.rawsPath "World.arf"

main :: IO ()
main = do

    cfg <- P.getDataFileName optionsPath >>= Cfg.loadConfiguration
    
    logFilePath <- Cfg.extract cfg logFileLoader   >>= P.getDataFileName
    worldPath   <- Cfg.extract cfg worldFileLoader >>= P.getDataFileName
    
    Log.setupLogger logFilePath
    Log.info $ "Logger started: " ++ logFilePath
    
    game <- loadGame worldPath
    Log.info $ "Game loaded from: " ++ worldPath
    
    boot cfg game
    
    Log.info "Game unloaded."
    Log.finish
