{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Application.Assets.GameFlow2 where

import View.Language
import View.Runtime
import View.View

import GameLogic.Facade

import qualified Application.Game.Engine.Runtime as Rt
import Application.Game.Engine.GameWire
import Application.Game.Engine.Core
import Application.Game.RuntimeSt

import Middleware.FRP.NetwireFacade as FRP
import Control.Wire.Unsafe.Event as UFRP

import Middleware.SDL.SDLFacade as SDL
import Prelude hiding (id, (.))

instance RuntimeSt Rt.GameStateTIO where
  getData = Rt.getData
  putData = Rt.putData

gameFlow2 :: GameWire () ()
gameFlow2 = printVal . switch w3

-- Event occures imideately:
w1 :: GameWire () Int
w1 = mkPure_ $ \_ -> Right 2

w2 :: GameWire () (Int, FRP.Event (GameWire () Int))
w2 = mkPure_ $ \_ -> Right (1, UFRP.Event w1)

-- The same:
w3 :: GameWire () (Int, FRP.Event (GameWire () Int))
w3 = pure 1 &&& now . w4

w4 :: GameWire () (GameWire () Int)
w4 = mkPure_ $ \_ -> Right w1
-- Expected: (Int, FRP.Event (GameWire () Int))
-- now :: Wire s e m a (Event a)

-- c :: (b, Event (Wire s e m a b))
-- switch :: (Monad m, Monoid s) => Wire s e m a (b, Event (Wire s e m a b)) -> Wire s e m a b
