module Application.Boot where

import qualified Middleware.Config.Facade as Cfg
import qualified Middleware.Tracing.Log as Log
import Middleware.SDL.Environment

import View.Config
import View.View

import Application.Game.Engine.Runtime
import Application.Game.Engine.Core
import Application.Assets.ConfigScheme
import Application.Config

import qualified CellularNet.Facade as Net

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
    


