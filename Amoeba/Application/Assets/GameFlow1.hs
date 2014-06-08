{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Application.Assets.GameFlow1 where

import View.Language
import View.Runtime
import View.View

import qualified Application.Game.Engine.Runtime as Rt
import Application.Game.Engine.GameWire
import Application.Game.Engine.Core
import Application.Game.RuntimeSt

import Middleware.FRP.NetwireFacade as FRP
import Middleware.SDL.SDLFacade as SDL
import Prelude hiding (id, (.))

instance RuntimeSt Rt.GameStateTIO where
  getData = Rt.getData
  putData = Rt.putData


-- This workflow just switches modes by Space.
-- It allows to move viewpoint by mouse.
-- To finish the application, close it in a usual way or press 'Esc'.

gameNode :: GameNode -> GameWire () ()
gameNode node = modes Render (selector node) .
            (
                pure () &&& now . interpreter node . pollSdlEvent
            )

switcher node w1 = mkEmpty . w1 --> gameNode node

selector _    Finish                     = quit . diagnose "Finish"
selector node Render                     = switcher node render
selector _   (SwitchNode swNode)         = switcher swNode render
selector node (StartViewPointMoving x y) = switcher node (render . startViewPointMoving . pure (x, y))
selector node (ViewPointMoving x y)      = switcher node (render . viewPointMoving . pure (x, y))
selector node (StopViewPointMoving x y)  = switcher node (render . stopViewPointMoving . pure (x, y))

interpreter :: GameNode -> GameWire SDL.Event Command
interpreter node = mkSF_ $ \e -> case e of
    SDL.Quit -> Finish
    (SDL.KeyDown (SDL.Keysym SDL.SDLK_ESCAPE _ _)) -> Finish
    (SDL.KeyDown (SDL.Keysym SDL.SDLK_SPACE _ _)) -> SwitchNode $ next node
    SDL.MouseButtonDown x y SDL.ButtonLeft -> StartViewPointMoving x y
    SDL.MouseMotion x y _ _ -> ViewPointMoving x y
    SDL.MouseButtonUp x y SDL.ButtonLeft -> StopViewPointMoving x y
    _ -> Render
    
next Screen4 = Screen1
next node = succ node

render = mkGen_ $ const $ do
    view <- Rt.getView
    dat <- getData
    withIO $ renderGame view dat

startViewPointMoving = mkGen_ $ \point -> do
    view <- Rt.getView
    let point' = toViewPoint point
    let view' = view {viewVirtualPlainShift = Just (point', point')}
    Rt.putView view'
    retR ()

viewPointMoving = mkGen_ $ \point -> do
    view <- Rt.getView
    let mbShift = viewVirtualPlainShift view
    case mbShift of
        Just (shiftStart, _) -> do
            let point' = toViewPoint point
            let view' = view {viewVirtualPlainShift = Just (shiftStart, point')}
            Rt.putView view'
            retR ()
        Nothing -> retR ()

stopViewPointMoving = mkGen_ $ \point ->do
    view@(View a b c vPlane mbShift) <- Rt.getView
    case mbShift of
        Just (p1, p2) -> do
            Rt.putView $ View a b c (vPlane +! p2 -! p1) Nothing
            retR ()
        Nothing -> retR ()