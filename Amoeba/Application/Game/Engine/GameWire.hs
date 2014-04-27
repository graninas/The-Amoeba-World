module Application.Game.Engine.GameWire where

import Middleware.FRP.NetwireFacade
import Application.Game.Engine.Runtime

type Inhibitor = String
type GTime = Timed NominalDiffTime ()
type GWire state a b = Wire GTime Inhibitor state a b
type GameWire a b = GWire GameStateTIO a b