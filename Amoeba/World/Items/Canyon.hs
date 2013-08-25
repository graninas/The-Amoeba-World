module World.Items.Canyon where

import World.World
import World.Player
import World.Geometry
import World.Stochastic
import World.Constants
import World.Types

import Data.Word
import System.Random
import qualified Data.List as L
import qualified Data.Either as E


data Canyon = Canyon { canyonId :: ItemId
                     , canyonOwner :: Player }

instance Id Canyon where
    getId = canyonId

instance Active Canyon where
    activate w p _ = inactive w p
    ownedBy = canyonOwner
    
    
canyon :: ItemId -> Player -> Point -> [(Point, Items)]
canyon cId pl p = [(p, makeItems [Canyon cId pl])]