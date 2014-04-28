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
gameNode node = modes (Render, Render) (processor node) .
            (
                pure () &&& now . interpreter node . pollSdlEvent
            )

process _ Finish = quit . trace "Finish"
process node Render = mkEmpty . render --> gameNode node
process _ (SwitchNode swNode) = mkEmpty . render . trace ("Switched to node: " ++ show swNode) --> gameNode swNode

processor :: GameNode -> (Command, Command) -> GameWire () ()
processor node (c1, c2) = mkEmpty . process node c1 --> process node c1

interpreter :: GameNode -> GameWire SDL.Event (Command, Command)
interpreter node = proc event -> do
    command1 <- interpretSpecific node -< event
    command2 <- interpretCommon -< event
    returnA -< (command1, command2)

interpretCommon :: GameWire SDL.Event Command
interpretCommon = mkGen_ $ \e -> case e of
    SDL.Quit -> retR Finish
    _ -> retR Render

interpretSpecific :: GameNode -> GameWire SDL.Event Command
interpretSpecific Screen1 = mkGen_ $ \e -> case e of
    (SDL.KeyDown k@(SDL.Keysym SDL.SDLK_SPACE _ _)) -> do
--        setKeyHolding k
        retR $ SwitchNode Screen2
    _ -> retR Render
interpretSpecific node = mkGen_ $ \e -> case e of
    (SDL.KeyDown (SDL.Keysym SDL.SDLK_SPACE _ _)) -> retR $ SwitchNode (next node)
    _ -> retR $ Render


next Screen4 = Screen1
next node = succ node

