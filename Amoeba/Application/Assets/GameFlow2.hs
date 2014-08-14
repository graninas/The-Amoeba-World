{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Application.Assets.GameFlow2 where

import View.Language
import Application.Assets.Wires
import Application.Game.Engine.GameWire
import Application.Game.Engine.Core

import Middleware.FRP.NetwireFacade as FRP
import Middleware.SDL.SDLFacade as SDL

import Prelude hiding (id, (.))

gameFlow2 :: GameWire () ()
gameFlow2 = printVal . switch w1

w1 :: GameWire () (SDL.Event, FRP.Event (GameWire () SDL.Event))
w1 = pure SDL.NoEvent &&& now . w2

w2 :: GameWire () (GameWire () SDL.Event)
w2 = mkPure_ $ \_ -> Right pollSdlEvent'
