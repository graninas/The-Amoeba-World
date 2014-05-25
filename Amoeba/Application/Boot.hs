module Application.Boot where

import View.Config
import View.View

import qualified Middleware.Config.Facade as Cfg
import qualified Middleware.Tracing.Log as Log
import Middleware.SDL.Environment

import Application.Game.Logic
import Application.Game.Engine.Runtime
import Application.Game.Engine.Core
import Application.Config
import Application.Storage.GameLoader

boot cfg = do
    logFilePath <- Cfg.extract cfg logFileLoader
    worldPath <- Cfg.extract cfg worldFileLoader
    
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
    


