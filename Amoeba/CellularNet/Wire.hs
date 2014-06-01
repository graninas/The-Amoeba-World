{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module CellularNet.Wire (logic) where

import qualified Application.Game.Engine.Runtime as Rt
import Application.Game.Engine.GameWire
import Application.Game.Engine.Core
import CellularNet.RuntimeSt
import CellularNet.View
import CellularNet.Net
import View.Runtime

import Middleware.FRP.NetwireFacade as FRP
import Middleware.SDL.SDLFacade as SDL
import Prelude hiding (id, (.))
import Control.Monad (liftM)
import Control.Monad.State (get, put)
import Data.Word (Word16)

instance RuntimeSt Rt.GameStateTIO where
  getData = Rt.getData
  putData = Rt.putData

data GameNode = Single | Periodic
  deriving (Ord, Eq, Show, Enum)

data Command = Finish
             | Render
             | Update
             | SwitchNode GameNode
             | StartViewPointMoving Word16 Word16
             | ViewPointMoving Word16 Word16
             | StopViewPointMoving Word16 Word16
  deriving (Ord, Eq, Show)

logic :: GameWire () ()
logic = gameNode Single

gameNode :: GameNode -> GameWire () ()
gameNode node = modes Render (selector node) .
            (
                pure () &&& metainterpreter node
            )

switcher node w1 = mkEmpty . w1 --> gameNode node

selector _     Finish                    = quit . diagnose "Finish"
selector _    (SwitchNode swNode)        = switcher swNode mkEmpty
selector node  Render                    = switcher node render
selector node  Update                    = switcher node (render . update)
selector node (StartViewPointMoving x y) = switcher node (render . startViewPointMoving . pure (x, y))
selector node (ViewPointMoving x y)      = switcher node (render . viewPointMoving      . pure (x, y))
selector node (StopViewPointMoving x y)  = switcher node (render . stopViewPointMoving  . pure (x, y))

metainterpreter Single   = now        . interpreter Single   . pollSdlEvent
metainterpreter Periodic = periodic 1 . interpreter Periodic . pollSdlEvent

interpreter :: GameNode -> GameWire SDL.Event Command
interpreter node = mkSF_ $ \e -> case e of
    SDL.Quit                                       -> Finish
    (SDL.KeyDown (SDL.Keysym SDL.SDLK_ESCAPE _ _)) -> Finish
    (SDL.KeyDown (SDL.Keysym SDL.SDLK_SPACE _ _))  -> SwitchNode Periodic
    SDL.MouseButtonDown x y SDL.ButtonLeft         -> StartViewPointMoving x y
    SDL.MouseMotion x y _ _                        -> ViewPointMoving x y
    SDL.MouseButtonUp   x y SDL.ButtonLeft         -> StopViewPointMoving x y
    _                                              -> Render

render = mkGen_ $ const $ do
    view <- Rt.getView
    net <- getData
    withIO $ renderNet view net
    
update = mkGen_ $ const $ do
    net <- getData
    let net' = stepFastNet net
    putData net'
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