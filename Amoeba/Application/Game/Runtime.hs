module Application.Game.Runtime where

import View.View
import GameLogic.Data.Facade
import Middleware.Config.Facade
import qualified Middleware.SDL.SDLFacade as SDL

import Control.Monad.State (get, StateT(..))
import Control.Monad (liftM)

data GameRt = GameRt { grtConfiguration :: Configuration
                     , grtView :: View
                     , grtGame :: Game
                     }

type GameStateTIO = StateT GameRt IO

runtime = GameRt

getWorld :: GameStateTIO World
getWorld = liftM (gWorld . grtGame) get

getSurface :: GameStateTIO ViewSurface
getSurface = liftM (viewSurface . grtView) get
