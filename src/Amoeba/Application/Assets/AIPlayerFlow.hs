module Amoeba.Application.Assets.AIPlayerFlow where

import Amoeba.Application.Game.Engine.GameWire

import Amoeba.Middleware.FRP.NetwireFacade as FRP

import Prelude hiding ((.), id)

aiPlayerFlow :: AIPlayerWire () ()
aiPlayerFlow = FRP.mkConst $ Right ()

