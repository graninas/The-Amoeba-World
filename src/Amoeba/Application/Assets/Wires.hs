{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Amoeba.Application.Assets.Wires where


import qualified Amoeba.GameLogic.Interpreter.Language as GL

import qualified Amoeba.Application.Game.Engine.Runtime as Rt
import Amoeba.Application.Game.Engine.GameWire
import Amoeba.Application.Game.Engine.Core
import Amoeba.Application.Game.RuntimeSt
import Amoeba.GameLogic.GameLogicAccessor as GLAcc
import Amoeba.View.ViewAccessor as ViewAcc
import Amoeba.View.Language
import Amoeba.View.Runtime
import Amoeba.View.Language as V

import Amoeba.Middleware.FRP.NetwireFacade as FRP
import Amoeba.Middleware.SDL.SDLFacade as SDL
import Prelude hiding (id, (.))
import Control.Monad.IO.Class (liftIO)

commandInterpreter :: GameNode -> ViewWire SDL.Event Command
commandInterpreter node = mkSF_ $ \e -> case e of
    SDL.Quit -> Finish
    (SDL.KeyDown (SDL.Keysym SDL.SDLK_ESCAPE _ _)) -> Finish
    (SDL.KeyDown (SDL.Keysym SDL.SDLK_RETURN _ _)) -> Update
    SDL.MouseButtonDown x y SDL.ButtonLeft -> StartViewPointMoving x y
    SDL.MouseMotion x y _ _ -> ViewPointMoving x y
    SDL.MouseButtonUp x y SDL.ButtonLeft -> StopViewPointMoving x y
    _ -> Render

{-
update = mkGen_ $ const $ do
    dat <- getData
    putData $ updateGame dat
    retR ()

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

-}

-- New approach research: STM
-- GameLogic wires:
finishGame = mkGen_ $ const $ do
    glAccessor <- Rt.getGameLogicAccessor
    withIO $ GLAcc.eval glAccessor GL.FinishGame

-- View wires:
render = mkGen_ $ const $ do
    view <- Rt.getView
    glAccessor <- Rt.getGameLogicAccessor
    withIO $ ViewAcc.eval (glAccessor, view) V.Render