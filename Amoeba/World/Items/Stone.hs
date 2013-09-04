module World.Items.Stone where

import World.World
import World.Player
import World.Geometry
import World.Stochastic
import World.Constants
import World.Descripted
import World.Types
import World.Id

import Data.Word
import System.Random
import qualified Data.List as L
import qualified Data.Either as E

data Stone = Stone { stoneId :: ItemId
                   , stoneOwner :: Player }
  deriving (Show, Read, Eq)

instance Id Stone where
    getId = stoneId

instance Active Stone where
    activate p i w = (w, [activationAnnotation p i])
    ownedBy = stoneOwner
    name _ = "Stone"
    
instance Descripted Stone where
    description = show
    
stone :: ItemId -> Player -> Point -> [(Point, Stone)]
stone sId pl p = [(p, Stone sId pl)]