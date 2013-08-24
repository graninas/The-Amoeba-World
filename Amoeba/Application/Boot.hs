module Application.Boot where

import Application.Environment
import Application.Constants
import Application.MainLoop

mainMenu = undefined

boot = withEnvironment $ do
    writeFile logFile "Log."
    setupScreen screenSettings "The Amoeba World"
    let startWorld = mainMenu
    startMainLoop mainLoopWire startWorld
    putStrLn "Be seen you..."