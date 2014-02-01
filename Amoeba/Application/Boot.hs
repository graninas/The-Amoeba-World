module Application.Boot where

import View.Config
import View.View
import qualified Middleware.Config.Facade as Cfg

import Application.Environment
import Application.Runtime.Engine
import Application.Runtime.Logic
import Application.Storage.WorldLoader

logPathLoader = Cfg.strOption Cfg.logPath
dataPathLoader = Cfg.strOption Cfg.dataPath

boot cfg = do
    logPath <- Cfg.extract cfg logPathLoader
    dataPath <- Cfg.extract cfg dataPathLoader

    game <- loadGame dataPath
    viewSettings <- loadViewSettings cfg
    withEnvironment $ do
        view <- setupView viewSettings
        putStrLn "Loaded."
        startMainLoop cfg view logic
        getLine
    putStrLn "Unloaded."
    


