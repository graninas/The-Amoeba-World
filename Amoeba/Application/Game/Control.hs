module Application.Game.Control where

import Control.Wire
import Control.Monad.State
import Prelude hiding ((.), id)

import Application.Game.Runtime

type WStateIO = StateT GameRt IO
type WWire a b = Wire (Timed NominalDiffTime ()) () WStateIO a b