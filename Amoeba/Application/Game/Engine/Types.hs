module Application.Game.Engine.Types where

import Middleware.FRP.NetwireFacade

type Inhibitor = String


type GWire state a b = Wire (Timed NominalDiffTime ()) Inhibitor state a b