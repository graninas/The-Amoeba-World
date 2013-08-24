module Application.Boot where

import Utils.Constants

import Game.Environment
import Game.MainLoop
import Game.Menu.Menu
import Game.World

import qualified Game.Wire as W
import qualified Game.Input.InputBuffer as I


mainMenu = Menu [ MenuItem "Generate new world"
                , MenuItem "Quit"
                ] 0 I.emptyInputBuffer

boot = withEnvironment $ do
    writeFile logFile "Log."
    setupScreen screenSettings "CurvedSpace"
    let startWorld = mainMenu
    startMainLoop mainLoopWire startWorld
    putStrLn "Be seen you..."