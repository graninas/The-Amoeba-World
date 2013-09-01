module Application.Boot where

import Application.Environment
import Application.Constants
import Application.MainLoop

boot startWorld = withEnvironment $ do
    writeFile logFile "Log."
    setupScreen screenSettings "The Amoeba World"
    startMainLoop mainLoopWire startWorld
    putStrLn "Be seen you..."