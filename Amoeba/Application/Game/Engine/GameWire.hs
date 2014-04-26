module Application.Game.Engine.GameWire where

import Middleware.FRP.NetwireFacade
import Application.Game.Engine.Runtime

type Inhibitor = String
type GWire state a b = Wire (Timed NominalDiffTime ()) Inhibitor state a b
type GameWire a b = GWire GameStateTIO a b