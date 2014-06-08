module Application.Game.Engine.Runtime where

import Application.Game.GameState
import View.Runtime
import Middleware.Config.Facade

import Control.Monad.State (get, put, StateT(..))
import Control.Monad.State.Class
import Control.Monad (liftM)

data GameRt = GameRt { grtConfiguration :: Configuration
                     , grtView :: View
                     , grtData :: GameState
                     }

type GameStateTIO = StateT GameRt IO

runtime = GameRt

getConfiguration :: GameStateTIO Configuration
getConfiguration = liftM grtConfiguration get

putConfiguration :: Configuration -> GameStateTIO ()
putConfiguration cfg = modify (\rt -> rt { grtConfiguration = cfg })

getView :: GameStateTIO View
getView = liftM grtView get

putView :: View -> GameStateTIO ()
putView view = modify (\rt -> rt { grtView = view })

getData :: GameStateTIO GameState
getData = liftM grtData get

putData :: GameState -> GameStateTIO ()
putData dat = modify (\rt -> rt { grtData = dat })