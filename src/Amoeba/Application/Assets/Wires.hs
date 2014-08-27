{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Amoeba.Application.Assets.Wires where

import Amoeba.View.Language
import Amoeba.View.Runtime
import Amoeba.View.View

import Amoeba.GameLogic.Facade

import qualified Amoeba.Application.Game.Engine.Runtime as Rt
import Amoeba.Application.Game.Engine.GameWire
import Amoeba.Application.Game.Engine.Core
import Amoeba.Application.Game.RuntimeSt

import Amoeba.Middleware.FRP.NetwireFacade as FRP
import Amoeba.Middleware.SDL.SDLFacade as SDL
import Prelude hiding (id, (.))

instance RuntimeSt Rt.GameStateTIO where
  getData = Rt.getData
  putData = Rt.putData

commandInterpreter :: GameNode -> GameWire SDL.Event Command
commandInterpreter node = mkSF_ $ \e -> case e of
    SDL.Quit -> Finish
    (SDL.KeyDown (SDL.Keysym SDL.SDLK_ESCAPE _ _)) -> Finish
    (SDL.KeyDown (SDL.Keysym SDL.SDLK_SPACE _ _)) -> SwitchNode $ next node
    (SDL.KeyDown (SDL.Keysym SDL.SDLK_RETURN _ _)) -> Update
    SDL.MouseButtonDown x y SDL.ButtonLeft -> StartViewPointMoving x y
    SDL.MouseMotion x y _ _ -> ViewPointMoving x y
    SDL.MouseButtonUp x y SDL.ButtonLeft -> StopViewPointMoving x y
    _ -> Render

-- This is for test only.
next Screen4 = TitleScreen
next node = succ node

render = mkGen_ $ const $ do
    view <- Rt.getView
    dat <- getData
    withIO $ renderGame view dat

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