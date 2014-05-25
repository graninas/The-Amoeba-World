module Application.Assets.GameFlow1 where

import View.Language
import Application.Game.Engine.Runtime
import Application.Game.Engine.GameWire
import Application.Game.Engine.Core

import Middleware.FRP.NetwireFacade as FRP
import Middleware.SDL.SDLFacade as SDL
import Prelude hiding (id, (.))

-- This workflow just switches modes by mouse click.
-- To finish the application, close it as usual.

gameNode :: GameNode -> GameWire () ()
gameNode node = modes Render (selector node) .
            (
                pure () &&& now . interpreter node . pollSdlEvent
            )

-- TODO: move to entry module.
selector _    Finish = quit . diagnose "Finish"
selector node Render = mkEmpty . render --> gameNode node
selector _   (SwitchNode swNode) = mkEmpty --> gameNode swNode
selector node Update = mkEmpty . render . update --> gameNode node
selector node (StartViewPointMoving x y) = mkEmpty . render . 
    startViewPointMoving . pure (x, y) . diagnose "Start view point moving." --> gameNode node
selector node (ViewPointMoving x y) = mkEmpty . render . 
    viewPointMoving . pure (x, y) --> gameNode node
selector node (StopViewPointMoving x y) = mkEmpty . render . 
    stopViewPointMoving . pure (x, y) . diagnose "Stop view point moving" --> gameNode node

-- TODO: move to entry module.
interpreter :: GameNode -> GameWire SDL.Event Command
interpreter node = mkSF_ $ \e -> case e of
    SDL.Quit -> Finish
    (SDL.KeyDown _) -> Update
    SDL.MouseButtonDown x y SDL.ButtonLeft -> StartViewPointMoving x y
    SDL.MouseMotion x y _ _ -> ViewPointMoving x y
    SDL.MouseButtonUp   x y SDL.ButtonLeft -> StopViewPointMoving x y
    _ -> Render

-- TODO: move to entry module.
next Screen4 = Screen1
next node = succ node