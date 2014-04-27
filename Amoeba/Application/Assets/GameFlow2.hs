module Application.Assets.GameFlow2 where

import View.Language
import Application.Game.Engine.Runtime
import Application.Game.Engine.GameWire
import Application.Game.Engine.Core

import Middleware.FRP.NetwireFacade as FRP
import Middleware.SDL.SDLFacade as SDL
import Prelude hiding (id, (.))

gameNode :: GameNode -> GameWire () ()
gameNode node = modes Render (selector node) .
            (
                pure () &&& now . interpreter node . pollSdlEvent
            )

selector _    Finish = quit . trace "Finish"
selector node Render = mkEmpty . render --> gameNode node
selector _   (SwitchNode swNode) = mkEmpty. trace "Switched." --> gameNode swNode

interpreter :: GameNode -> GameWire SDL.Event Command
interpreter node = mkSF_ $ \e -> case e of
    SDL.Quit -> Finish
    SDL.MouseButtonDown{} -> SwitchNode (next node)
    _ -> Render

next Screen4 = Screen1
next node = succ node