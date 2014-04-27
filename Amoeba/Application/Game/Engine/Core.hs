module Application.Game.Engine.Core where

import Application.Game.Engine.Runtime
import Application.Game.Engine.GameWire
import View.View

import Middleware.FRP.NetwireFacade hiding ((.))
import Middleware.SDL.SDLFacade as SDL
import Middleware.Tracing.ErrorHandling
import qualified Middleware.Tracing.Log as Log

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State (runStateT)

-- Main loop
startMainLoop :: GameWire () () -> GameRt -> IO (Inhibitor, GameRt)
startMainLoop wire = runStateT (startLoop wire)

startLoop = loop' clockSession_ (Right ())

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

forget = mkConst (Right ())

timeD :: (HasTime t s, Monad m) => Wire s e m a Double
timeD = fmap realToFrac time


-- Work wires
pollSdlEvent :: GameWire () SDL.Event
pollSdlEvent = mkGen_ $ \_ -> do
        e <- liftIO SDL.pollEvent
        retR e

-- TODO: make it safe on a type-level. Either or Maybe is needed.
render :: GameWire a ()
render = mkGen_ $ \_ -> do
    surf <- getSurface
    withLogError (clearScreen surf) "clearScreen failed."
    w <- getWorld
    withIO $ renderWorld surf w
    withIO $ SDL.flip surf