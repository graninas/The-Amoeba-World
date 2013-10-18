module GameLogic.ObjectCell where

import qualified GameLogic.GenericWorld as GW
import GameLogic.Object as O

instance GW.GenericCell Object where
    empty = O.empty
    merge = O.merge