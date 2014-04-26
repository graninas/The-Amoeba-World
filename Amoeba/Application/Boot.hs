module Application.Boot where

import View.Config
import View.View

import qualified Middleware.Config.Facade as Cfg
import qualified Middleware.Tracing.Log as Log
import Middleware.SDL.Environment

import Application.Game.Logic
import Application.Game.Engine.Runtime
import Application.Game.Engine.Core
import Application.Storage.GameLoader

logFileLoader = Cfg.filePathLoader Cfg.logPath "Amoeba.log"

-- TODO: move it in config file?
worldFileLoader = Cfg.filePathLoader Cfg.rawsPath "World.arf"

-- TOOD: this function should be in Either monad.
boot cfg = do
    -- TODO: Improve simple logging.
    logFilePath <- Cfg.extract cfg logFileLoader
    Log.setupLogger logFilePath
    Log.info $ "Logger started: " ++ logFilePath
    
    worldPath <- Cfg.extract cfg worldFileLoader
    game <- loadGame worldPath
    Log.info "Game loaded."
    
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
    


