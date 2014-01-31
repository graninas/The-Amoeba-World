module Application.Boot where

import View.Config
import View.View
import Middleware.Config.Facade

import Application.Environment
import Application.Runtime.Engine
import Application.Runtime.Logic

boot cfg = do
    viewSettings <- loadViewSettings cfg
    withEnvironment $ do
        view <- setupView viewSettings
        putStrLn "Loaded."
        startMainLoop cfg view logic
        getLine
    putStrLn "Unloaded."
    
loadViewSettings cfg = do
    screen <- extract cfg screenInfo
    caption <- extract cfg captionInfo
    print screen
    print caption
    return (screen, caption)
    


    
