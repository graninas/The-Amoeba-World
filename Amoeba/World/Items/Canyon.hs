module World.Items.Canyon where

import World.World
import World.Player
import World.Geometry
import World.Stochastic
import World.Constants
import World.Types
import World.Id

import System.Random
import qualified Data.List as L
import qualified Data.Either as E

data Canyon = Canyon { canyonId :: ItemId
                     , canyonOwner :: Player }
  deriving (Show, Read)

instance Id Canyon where
    getId = canyonId

instance Active Canyon where
    activate = inactive
    ownedBy = canyonOwner
    
instance Descripted Canyon where
    description = show

canyon :: ItemId -> Player -> Point -> [(Point, Canyon)]
canyon cId pl p = [(p, Canyon cId pl)]