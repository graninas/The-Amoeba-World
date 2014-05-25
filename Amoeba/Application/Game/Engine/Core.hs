module Application.Game.Engine.Core where

import Application.Game.Engine.Runtime
import Application.Game.Engine.GameWire
import View.NetView

-- This looks wrong.
import View.Runtime

import Middleware.FRP.NetwireFacade hiding ((.))
import Middleware.SDL.SDLFacade as SDL
import Middleware.Tracing.ErrorHandling
import qualified Middleware.Tracing.Log as Log

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State (runStateT)

-- Main loop
startMainLoop :: GameWire () () -> GameRt -> IO (Inhibitor, GameRt)
startMainLoop wire = runStateT (startLoop wire)

startLoop :: GameWire () () -> GameStateTIO Inhibitor
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

-- TODO: move to entry module.
render :: GameWire a ()
render = mkGen_ $ \_ -> do
    view <- getView
    net <- getNet
    withIO $ renderNet view net

-- TODO: use not a mkGen_.
-- TODO: move to entry module or introduce specific function 'updateState' in entry module.
update :: GameWire a ()
update = mkGen_ $ \_ -> do
    net <- getNet
    let net' = stepNet net
    putNet net'
    retR ()

-- TODO: use not a mkGen_.
startViewPointMoving :: GameWire ScreenPoint ()
startViewPointMoving = mkGen_ $ \point -> do
    view <- getView
    let point' = toViewPoint point
    let view' = view {viewVirtualPlainShift = Just (point', point')}
    putView view'
    retR ()

-- TODO: use not a mkGen_.
viewPointMoving :: GameWire ScreenPoint ()
viewPointMoving = mkGen_ $ \point -> do
    view <- getView
    let mbShift = viewVirtualPlainShift view
    case mbShift of
        Just (shiftStart, _) -> do
            let point' = toViewPoint point
            let view' = view {viewVirtualPlainShift = Just (shiftStart, point')}
            putView view'
            retR ()
        Nothing -> retR ()

-- TODO: use not a mkGen_.
stopViewPointMoving :: GameWire ScreenPoint ()
stopViewPointMoving = mkGen_ $ \point -> do
    view@(View a b c vPlane mbShift) <- getView
    case mbShift of
        Just (p1, p2) -> do
            putView $ View a b c (vPlane +! p2 -! p1) Nothing
            retR ()
        Nothing -> retR ()
