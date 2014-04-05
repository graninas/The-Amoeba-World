module Application.Game.Engine where

import Application.Game.Runtime
import Application.Game.Control
import GameLogic.Data.Game
import View.View
import Middleware.Config.Facade

import Control.Wire
import Control.Monad.State
import Prelude hiding ((.), id)

type Inhibitor = String

type GameStateTIO = StateT GameRt IO
type GWire a b = Wire (Timed NominalDiffTime ()) Inhibitor GameStateTIO a b

startMainLoop :: GWire () () -> Configuration -> View -> Game -> IO (Inhibitor, GameRt)
startMainLoop wire c v g = runStateT (startLoop wire) rt
  where
    rt = runtime c v g

startLoop = loop' clockSession_ (Right ())

loop' _ (Left res) _ = return res
loop' s input w = do
    (delta, s') <- stepSession s
    (eitherResult, w') <- stepWire w delta input
    loop' s' eitherResult w'