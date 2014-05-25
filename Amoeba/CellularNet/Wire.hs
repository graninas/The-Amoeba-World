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

data GameNode = Screen1 | Screen2 | Screen3 | Screen4
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
logic = gameNode Screen1

gameNode :: GameNode -> GameWire () ()
gameNode node = modes Render (selector node) .
            (
                pure () &&& now . interpreter node . pollSdlEvent
            )

selector _    Finish = quit . diagnose "Finish"
selector node Render = mkEmpty . render --> gameNode node
selector _   (SwitchNode swNode) = mkEmpty --> gameNode swNode
selector node Update = mkEmpty . render . update --> gameNode node
selector node (StartViewPointMoving x y) = mkEmpty . render . 
    startViewPointMoving . pure (x, y) --> gameNode node
selector node (ViewPointMoving x y) = mkEmpty . render . 
    viewPointMoving . pure (x, y) --> gameNode node
selector node (StopViewPointMoving x y) = mkEmpty . render . 
    stopViewPointMoving . pure (x, y) --> gameNode node

interpreter :: GameNode -> GameWire SDL.Event Command
interpreter node = mkSF_ $ \e -> case e of
    SDL.Quit -> Finish
    (SDL.KeyDown _) -> Update
    SDL.MouseButtonDown x y SDL.ButtonLeft -> StartViewPointMoving x y
    SDL.MouseMotion x y _ _ -> ViewPointMoving x y
    SDL.MouseButtonUp   x y SDL.ButtonLeft -> StopViewPointMoving x y
    _ -> Render

next Screen4 = Screen1
next node = succ node

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