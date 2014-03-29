module Application.Boot where

import View.Config
import View.View
import qualified Middleware.Config.Facade as Cfg
import qualified Middleware.Tracing.Log as Log

import Application.Environment
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
    Log.setupLogger logFilePath
    Log.debug "Logger started."
    
    worldPath <- Cfg.extract cfg worldFileLoader
    game <- loadGame worldPath
    Log.debug "Game loaded."
    
    viewSettings <- loadViewSettings cfg
    Log.debug "View settings loaded."
    
    withEnvironment $ do
        view <- setupView viewSettings
        Log.debug "View prepared."
        startMainLoop cfg view game logic
        getLine
    
    Log.debug "Game unloaded."
    


