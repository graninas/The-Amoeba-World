module Amoeba.Application.Game.Engine.Core where

import Amoeba.Application.Game.Engine.Runtime
import Amoeba.Application.Game.Engine.GameWire
import Amoeba.View.View

import Amoeba.Middleware.FRP.NetwireFacade hiding ((.))
import Amoeba.Middleware.SDL.SDLFacade as SDL
import Amoeba.Middleware.Tracing.ErrorHandling
import qualified Amoeba.Middleware.Tracing.Log as Log

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.State (runStateT)

-- Main loop
startMainLoop :: GameWire () () -> GameRt -> IO (Inhibitor, GameRt)
startMainLoop wire = runStateT (startLoop wire)

startLoop :: GameWire () () -> GameStateTIO Inhibitor
startLoop = loop' clockSession_ (Right ())

loop' :: Session GameStateTIO (Timed NominalDiffTime ()) -> Either Inhibitor () -> GameWire () () -> GameStateTIO Inhibitor
loop' _ (Left res) _ = return res
loop' s input w = do
    (delta, s') <- stepSession s
    (eitherResult, w') <- stepWire w delta input
    loop' s' eitherResult w'

-- Combinator wires
quitWith = inhibit
quit = inhibit "Finished."

retR = return . Right
withIO ioAct = liftIO ioAct >> retR ()

diagnose :: Show a => a -> GameWire () ()
diagnose a = mkGen_ $ \_ -> withIO . print $ a

trace :: Show a => a -> GameWire () ()
trace a = mkGen_ $ \_ -> withIO . Log.info . show $ a

printVal :: Show a => GameWire a ()
printVal = mkGen_ $ \a -> withIO . print $ a

forget = mkConst (Right ())

timeD :: (HasTime t s, Monad m) => Wire s e m a Double
timeD = fmap realToFrac time


-- Work wires
pollSdlEvent :: GameWire () SDL.Event
pollSdlEvent = mkGen_ $ \_ -> do
        e <- liftIO SDL.pollEvent
        retR e

pollSdlEvent' :: GameWire () SDL.Event
pollSdlEvent' = mkGen_ $ \_ -> do
        e <- liftIO SDL.pollEvent
        liftIO pumpEvents
        retR e