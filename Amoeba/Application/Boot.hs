module Application.Boot where

import Application.Environment
import View.Config
import View.View
import Middleware.Config.Facade

loadViewSettings cfg = do
    screen <- extract cfg screenInfo
    caption <- extract cfg captionInfo
    print screen
    print caption
    return (screen, caption)

boot cfg = do
    (screen, caption) <- loadViewSettings cfg
    withEnvironment $ do
        view <- setupScreen screen caption
        putStrLn "Loaded."
        getLine
    putStrLn "Unloaded."
    
    