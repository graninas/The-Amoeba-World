module Application.Boot where

import View.Config
import View.View
import qualified Middleware.Config.Facade as Cfg
import qualified Middleware.Tracing.Log as Log
import Middleware.SDL.Environment

import Application.Game.Engine
import Application.Game.Logic
import Application.Storage.GameLoader

logFileLoader = Cfg.filePathLoader Cfg.logPath "Amoeba.log"

-- TODO: move it in config file?
worldFileLoader = Cfg.filePathLoader Cfg.rawsPath "World.arf"

-- TOOD: this function should be in Either monad.
boot cfg = do
    -- TODO: Improve simple logging.
    logFilePath <- Cfg.extract cfg logFileLoader
    putStrLn $ logFilePath
    Log.setupLogger logFilePath
    Log.info "Logger started."
    
    worldPath <- Cfg.extract cfg worldFileLoader
    game <- loadGame worldPath
    Log.info "Game loaded."
    
    viewSettings <- loadViewSettings cfg
    Log.info "View settings loaded."
    
    withEnvironment $ do
        view <- setupView viewSettings
        Log.info "View prepared."
        (inhibitor, _) <- startMainLoop logic cfg view game
        Log.info $ "Inhibitor: " ++ inhibitor
    
    Log.info "Game unloaded."
    Log.finish
    


