module Application.Game.Engine.Runtime where

import View.Runtime
import CellularNet.Net
import Middleware.Config.Facade

import Control.Monad.State (get, put, StateT(..))
import Control.Monad (liftM)

data GameRt = GameRt { grtConfiguration :: Configuration
                     , grtView :: View
                     , grtNet :: FastNet
                     }

type GameStateTIO = StateT GameRt IO

runtime = GameRt

getNet :: GameStateTIO FastNet
getNet = liftM grtNet get

getView :: GameStateTIO View
getView = liftM grtView get

putNet :: FastNet -> GameStateTIO ()
putNet net = do
    rt <- get
    put $ rt { grtNet = net }