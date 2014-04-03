module Application.Game.Engine where

import Application.Game.Runtime
import Application.Game.Control
import GameLogic.Data.Game
import View.View
import Middleware.Config.Facade

import Control.Wire
import Control.Monad.State
import Prelude hiding ((.), id)

startMainLoop :: Configuration -> View -> Game -> WWire () ExecutionResult -> IO ExecutionResult
startMainLoop cfg view game wire = do
    let rt = runtime cfg view game
    (s, session') <- stepSession clockSession_
    --let state = gameLoop wire s Running
    --(val, _) <- runStateT state rt
    --return val
    undefined
