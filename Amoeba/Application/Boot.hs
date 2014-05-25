module Application.Boot where

import View.Config
import View.View

import qualified Middleware.Config.Facade as Cfg
import qualified Middleware.Tracing.Log as Log
import Middleware.SDL.Environment

import Application.Game.Engine.Runtime
import Application.Game.Engine.Core

import qualified CellularNet.Facade as Net

logFileLoader = Cfg.filePathLoader Cfg.logPath "Endo.log"

boot cfg = do
    logFilePath <- Cfg.extract cfg logFileLoader
    Log.setupLogger logFilePath
    Log.info $ "Logger started: " ++ logFilePath
    
    dat <- Net.load cfg
    Log.info "Net loaded."
    
    viewSettings <- loadViewSettings cfg
    Log.info "View settings loaded."
    
    withEnvironment $ do
        view <- setupView viewSettings
        Log.info "View prepared."
        let rt = runtime cfg view dat
        (inhibitor, _) <- startMainLoop Net.logic rt
        Log.info $ "Inhibitor: " ++ if null inhibitor then "Unspecified." else inhibitor
    
    Log.info "Game unloaded."
    Log.finish
    


