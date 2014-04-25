module Application.Game.Engine.Core where

import Middleware.FRP.NetwireFacade hiding ((.))

import Control.Monad.IO.Class (liftIO)

quitWith = inhibit
quit = inhibit "Finished."

withIO ioAct = liftIO ioAct >> return (Right ())
