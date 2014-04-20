module Application.Game.Engine.Core where

import Middleware.FRP.NetwireFacade

import Control.Monad.IO.Class (liftIO)

quit = mkConst $ Left "Finished."

withIO ioAct = liftIO ioAct >> return (Right ())