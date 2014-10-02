{-# LANGUAGE LambdaCase #-}
module Amoeba.Application.Assets.ViewFlow where

import Amoeba.View.Facade as V
import Amoeba.Application.Game.Engine.GameWire
import Amoeba.Application.Game.Engine.Runtime as Rt
import Amoeba.GameLogic.GameLogicAccessor as GLAcc
import Amoeba.View.ViewAccessor as ViewAcc
import Amoeba.GameLogic.Facade as GL

import Amoeba.Middleware.FRP.NetwireFacade as FRP
--import Amoeba.Middleware.GLFW.Facade as GLFW

import qualified Amoeba.Middleware.Tracing.Log as Log

import Prelude hiding (id, (.))
import Control.Monad.IO.Class (liftIO)

-- TODO: start screen should be defined correctly.
viewFlow :: ViewWire () ()
viewFlow = viewFlow' TitleScreen

viewFlow' :: GameNode -> ViewWire () ()
viewFlow' node = modes Render (selector node) .
    (
        pure () &&& now . commandInterpreter node . event
    )

switcher :: GameNode -> ViewWire () () -> ViewWire () ()
switcher node w1 = mkEmpty . w1 --> viewFlow' node

selector :: GameNode -> V.Command -> ViewWire () ()
selector _    V.Finish    = quit . finishGame . diagnose "Manually finished."
selector node V.Render    = switcher node render
selector node viewCommand = switcher node (render . evalViewCommand' viewCommand)

finishGame = mkGen_ $ const $ do
    glAccessor <- Rt.getViewGameLogicAccessor
    withIO $ GLAcc.eval glAccessor GL.FinishGame

evalViewCommand' command = mkGen_ $ const $ do
    accessors <- Rt.getAccessors
    withIO $ ViewAcc.evalViewCommand accessors command

render = evalViewCommand' V.Render

commandInterpreter :: GameNode -> ViewWire ViewAcc.Event V.Command
commandInterpreter node = mkSF_ $ \case
    ViewAcc.EventClose -> V.Finish
{-    (SDL.KeyDown (SDL.Keysym SDL.SDLK_ESCAPE _ _)) -> Finish
    SDL.MouseButtonDown x y SDL.ButtonLeft         -> StartViewPointMoving (x, y)
    SDL.MouseMotion x y _ _                        -> ViewPointMoving (x, y)
    SDL.MouseButtonUp x y SDL.ButtonLeft           -> StopViewPointMoving (x, y)
-}
    _ -> V.Render

mkViewAccessorWire act = mkGen_ $ const $ do
    viewAccessor <- Rt.getViewAccessor
    v <- liftIO $ act viewAccessor
    retR v

event :: ViewWire () ViewAcc.Event
event = mkViewAccessorWire ViewAcc.getEvent

-- debug
diagnose :: Show a => a -> ViewWire () ()
diagnose a = mkGen_ $ \_ -> withIO . print $ a

trace :: Show a => a -> ViewWire () ()
trace a = mkGen_ $ \_ -> withIO . Log.info . show $ a

printVal :: Show a => ViewWire a a
printVal = mkGen_ $ \a -> (liftIO $ print a) >> (liftIO $ Log.info $ show a)  >> retR a 