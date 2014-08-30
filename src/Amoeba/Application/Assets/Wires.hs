{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Amoeba.Application.Assets.Wires where


import Amoeba.Application.Game.Engine.Runtime as Rt
import Amoeba.Application.Game.Engine.GameWire
import Amoeba.Application.Game.Engine.Core
import Amoeba.GameLogic.GameLogicAccessor as GLAcc
import Amoeba.GameLogic.Interpreter.Language as GL
import Amoeba.View.ViewAccessor as ViewAcc
import Amoeba.View.Language as V

import Amoeba.Middleware.FRP.NetwireFacade as FRP
import Amoeba.Middleware.SDL.SDLFacade as SDL
import Prelude hiding (id, (.))

commandInterpreter :: GameNode -> ViewWire SDL.Event V.Command
commandInterpreter node = mkSF_ $ \e -> case e of
    SDL.Quit -> Finish
    (SDL.KeyDown (SDL.Keysym SDL.SDLK_ESCAPE _ _)) -> Finish
    SDL.MouseButtonDown x y SDL.ButtonLeft         -> StartViewPointMoving (x, y)
    SDL.MouseMotion x y _ _                        -> ViewPointMoving (x, y)
    SDL.MouseButtonUp x y SDL.ButtonLeft           -> StopViewPointMoving (x, y)
    _ -> Render

-- New approach research: STM for View flow
-- GameLogic wires:
finishGame = mkGen_ $ const $ do
    glAccessor <- Rt.getViewGameLogicAccessor
    withIO $ GLAcc.eval glAccessor GL.FinishGame

-- View wires:
evalViewCommand command = mkGen_ $ const $ do
    viewAccessor <- Rt.getViewAccessor
    glAccessor   <- Rt.getViewGameLogicAccessor
    withIO $ ViewAcc.eval (glAccessor, viewAccessor) command

render = evalViewCommand V.Render

