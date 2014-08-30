module Amoeba.Application.Game.Engine.Core where

-- TODO: remove bad dependency from View
-- Needs generalization.
import Amoeba.Application.Game.Engine.Runtime
import Amoeba.Application.Game.Engine.GameWire
import Amoeba.View.View

import Amoeba.Middleware.FRP.NetwireFacade hiding ((.))
import Amoeba.Middleware.SDL.SDLFacade as SDL
import Amoeba.Middleware.Tracing.ErrorHandling
import qualified Amoeba.Middleware.Tracing.Log as Log

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.State (get, put, StateT(..))
import Control.Monad.State.Class
import Control.Monad.Trans.State (runStateT)

-- TODO: HACK: FIXME: needs deep generalization of looping and wires mechanism.
-- Will do it later. Now, this is a HACK and copy-paste.

-- View Wire loop.
startMainLoopView :: ViewWire () () -> ViewRt -> IO (Inhibitor, ViewRt)
startMainLoopView wire = runStateT (startLoopView wire)

startLoopView :: ViewWire () () -> ViewTIO Inhibitor
startLoopView = loopView' clockSession_ (Right ())

loopView' :: Session ViewTIO (Timed NominalDiffTime ())
      -> Either Inhibitor ()
      -> ViewWire () ()
      -> ViewTIO Inhibitor
loopView' _ (Left res) _ = return res
loopView' s input w = do
    (delta, s') <- stepSession s
    (eitherResult, w') <- stepWire w delta input
    loopView' s' eitherResult w'


-- GameStorage Wire loop
startMainLoopGS :: GameStorageWire () () -> GameStorageRt -> IO (Inhibitor, GameStorageRt)
startMainLoopGS wire = runStateT (startLoopGS wire)

startLoopGS :: GameStorageWire () () -> GameStorageTIO Inhibitor
startLoopGS = loopGS' clockSession_ (Right ())

loopGS' :: Session GameStorageTIO (Timed NominalDiffTime ())
      -> Either Inhibitor ()
      -> GameStorageWire () ()
      -> GameStorageTIO Inhibitor
loopGS' _ (Left res) _ = return res
loopGS' s input w = do
    (delta, s') <- stepSession s
    (eitherResult, w') <- stepWire w delta input
    loopGS' s' eitherResult w'

-- AI Player Wire loop
startMainLoopAI :: AIPlayerWire () () -> AIPlayerRt -> IO (Inhibitor, AIPlayerRt)
startMainLoopAI wire = runStateT (startLoopGS wire)

startLoopAI :: AIPlayerWire () () -> AIPlayerTIO Inhibitor
startLoopAI = loopAI' clockSession_ (Right ())

loopAI' :: Session AIPlayerTIO (Timed NominalDiffTime ())
      -> Either Inhibitor ()
      -> AIPlayerWire () ()
      -> AIPlayerTIO Inhibitor
loopAI' _ (Left res) _ = return res
loopAI' s input w = do
    (delta, s') <- stepSession s
    (eitherResult, w') <- stepWire w delta input
    loopAI' s' eitherResult w'

--End of HACK

-- Combinator wires
quitWith = inhibit
quit = inhibit "Finished."

retR = return . Right
withIO ioAct = liftIO ioAct >> retR ()

diagnose :: Show a => a -> ViewWire () ()
diagnose a = mkGen_ $ \_ -> withIO . print $ a

trace :: Show a => a -> ViewWire () ()
trace a = mkGen_ $ \_ -> withIO . Log.info . show $ a

printVal :: Show a => ViewWire a ()
printVal = mkGen_ $ \a -> withIO . print $ a

forget = mkConst (Right ())

timeD :: (HasTime t s, Monad m) => Wire s e m a Double
timeD = fmap realToFrac time


-- Work wires
-- TODO: remove it from here.
pollSdlEvent :: ViewWire () SDL.Event
pollSdlEvent = mkGen_ $ \_ -> do
        e <- liftIO SDL.pollEvent
        retR e

pollSdlEvent' :: ViewWire () SDL.Event
pollSdlEvent' = mkGen_ $ \_ -> do
        e <- liftIO SDL.pollEvent
        liftIO pumpEvents
        retR e