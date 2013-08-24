module Application.Wire
    ( module Control.Wire
    , module Control.Monad
    , GameWire(..)
    , Inhibitor(..)
    ) where

import Control.Wire
import Control.Monad hiding (unless, when)
import Prelude hiding ((.), id)

type Inhibitor = ()
type GameWire w = Wire Inhibitor IO w w
        