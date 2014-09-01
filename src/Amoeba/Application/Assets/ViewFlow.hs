module Amoeba.Application.Assets.ViewFlow where

import Amoeba.View.Facade
import Amoeba.Application.Assets.Wires
import Amoeba.Application.Game.Engine.GameWire

import Amoeba.Middleware.FRP.NetwireFacade as FRP
import Amoeba.Middleware.SDL.SDLFacade as SDL
import qualified Amoeba.Middleware.Tracing.Log as Log

import Prelude hiding (id, (.))
import Control.Monad.IO.Class (liftIO)

-- TODO: start screen should be defined correctly.
viewFlow = viewFlow' TitleScreen

viewFlow' :: GameNode -> ViewWire () ()
viewFlow' node = modes Render (selector node) .
            (
                pure () &&& now . commandInterpreter node . pollSdlEvent
            )

switcher node w1 = mkEmpty . w1 --> viewFlow' node

selector node Finish      = quit . finishGame . diagnose "Manually finished."
selector node Render      = switcher node  render
selector node viewCommand = switcher node (render . evalViewCommand viewCommand)

pollSdlEvent :: ViewWire () SDL.Event
pollSdlEvent = mkGen_ $ \_ -> do
        e <- liftIO SDL.pollEvent
        retR e

pollSdlEvent' :: ViewWire () SDL.Event
pollSdlEvent' = mkGen_ $ \_ -> do
        e <- liftIO SDL.pollEvent
        liftIO pumpEvents
        retR e
        
-- debug
diagnose :: Show a => a -> ViewWire () ()
diagnose a = mkGen_ $ \_ -> withIO . print $ a

trace :: Show a => a -> ViewWire () ()
trace a = mkGen_ $ \_ -> withIO . Log.info . show $ a

printVal :: Show a => ViewWire a ()
printVal = mkGen_ $ \a -> withIO . print $ a
