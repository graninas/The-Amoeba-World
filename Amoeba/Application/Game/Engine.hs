module Application.Game.Engine where

import Application.Game.Runtime
import Application.Game.Control
import GameLogic.Data.Game
import View.View
import Middleware.Config.Facade

import Control.Wire
import Control.Monad.State
import Prelude hiding ((.), id)

startMainLoop :: Configuration -> View -> Game -> WWire Int Int -> IO ()
startMainLoop cfg view game wire = do
    let rt = runtime cfg view game
    (s, session') <- stepSession clockSession_

    -- TODO: what is this??!
    let state = gameLoop wire s (Right 10)
    let newState = execStateT state rt
    return ()

gameLoop wire s input = do
    (output, w') <- stepWire wire s input
    case output of
        Left ex -> return 10
        Right x -> return 20

