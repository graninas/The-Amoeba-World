module Amoeba.Application.Boot where

import Amoeba.View.Config
import Amoeba.View.View

import qualified Amoeba.Middleware.Config.Facade as Cfg
import qualified Amoeba.Middleware.Tracing.Log as Log
import Amoeba.Middleware.SDL.Environment

import Amoeba.Application.Game.Logic
import Amoeba.Application.Game.Engine.Runtime
import Amoeba.Application.Game.Engine.Core
import Amoeba.Application.Config
import Amoeba.Application.Storage.GameLoader

import Paths_The_Amoeba_World as P

boot cfg = do
    logFilePath <- Cfg.extract cfg logFileLoader   >>= P.getDataFileName
    worldPath   <- Cfg.extract cfg worldFileLoader >>= P.getDataFileName
    
    Log.setupLogger logFilePath
    Log.info $ "Logger started: " ++ logFilePath
    game <- loadGame worldPath
    Log.info $ "Game loaded from: " ++ worldPath
    
    viewSettings <- loadViewSettings cfg
    Log.info "View settings loaded."
    
    withEnvironment $ do
        view <- setupView viewSettings
        Log.info "View prepared."
        let rt = runtime cfg view game
        (inhibitor, _) <- startMainLoop logic rt
        Log.info $ "Inhibitor: " ++ if null inhibitor then "Unspecified." else inhibitor
    
    Log.info "Game unloaded."
    Log.finish
    


