module Application.Game.Engine.MainLoop where

import Application.Game.Engine.Types
import Application.Game.Runtime

import Middleware.FRP.NetwireFacade
import Control.Monad.State (runStateT)

startMainLoop :: GWire GameStateTIO () () -> GameRt -> IO (Inhibitor, GameRt)
startMainLoop wire = runStateT (startLoop wire)

startLoop = loop' clockSession_ (Right ())

loop' _ (Left res) _ = return res
loop' s input w = do
    (delta, s') <- stepSession s
    (eitherResult, w') <- stepWire w delta input
    loop' s' eitherResult w'