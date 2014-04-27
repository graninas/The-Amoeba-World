{-# LANGUAGE Arrows #-}
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
selector _   (SwitchNode swNode) = mkEmpty . render . trace ("Switched to node: " ++ show swNode) --> gameNode swNode

interpreter :: GameNode -> GameWire SDL.Event Command
interpreter node = proc event -> do
    command <- interpretSpecific node -< event
    returnA -< command

interpretSpecific :: GameNode -> GameWire SDL.Event Command
interpretSpecific Screen1 = mkGen_ $ \e -> case e of
    SDL.Quit -> retR Finish
    (SDL.KeyDown k@(SDL.Keysym SDL.SDLK_SPACE _ _)) -> do
--        setKeyHolding k
        retR $ SwitchNode Screen2
    _ -> retR Render
interpretSpecific node = mkGen_ $ \e -> case e of
    SDL.Quit -> retR Finish
    (SDL.KeyDown (SDL.Keysym SDL.SDLK_SPACE _ _)) -> retR $ SwitchNode (next node)
    _ -> retR $ Render


next Screen4 = Screen1
next node = succ node

