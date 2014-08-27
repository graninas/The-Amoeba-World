{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Amoeba.Application.Assets.GameFlow2 where

import Amoeba.View.Language
import Amoeba.Application.Assets.Wires
import Amoeba.Application.Game.Engine.GameWire
import Amoeba.Application.Game.Engine.Core

import Amoeba.Middleware.FRP.NetwireFacade as FRP
import Amoeba.Middleware.SDL.SDLFacade as SDL

import Prelude hiding (id, (.))

gameFlow2 :: GameWire () ()
gameFlow2 = printVal . switch w1

w1 :: GameWire () (SDL.Event, FRP.Event (GameWire () SDL.Event))
w1 = pure SDL.NoEvent &&& now . w2

w2 :: GameWire () (GameWire () SDL.Event)
w2 = mkPure_ $ \_ -> Right pollSdlEvent'
