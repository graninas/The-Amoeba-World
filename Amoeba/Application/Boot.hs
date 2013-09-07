module Application.Boot where

import Application.Environment
import Application.Constants
import Application.MainLoop

boot world = withEnvironment $ do
    writeFile logFile "Log."
    startMainLoop mainWire world
    putStrLn "Be seen you..."