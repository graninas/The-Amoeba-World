module Amoeba.Application.Assets.AIPlayerFlow where

import Amoeba.Application.Assets.Wires
import Amoeba.Application.Game.Engine.GameWire

import Amoeba.Middleware.FRP.NetwireFacade as FRP
import Amoeba.Middleware.SDL.SDLFacade as SDL
import qualified Amoeba.Middleware.Tracing.Log as Log

aiPlayerFlow :: AIPlayerWire () ()
aiPlayerFlow = FRP.mkConst $ Right ()
