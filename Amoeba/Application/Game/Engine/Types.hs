module Application.Game.Engine.Types where

import Middleware.FRP.NetwireFacade

import Control.Monad.State (StateT(..))

type Inhibitor = String

type GameStateTIO runtime = StateT runtime IO
type GWire runtime a b = Wire (Timed NominalDiffTime ()) Inhibitor (GameStateTIO runtime) a b