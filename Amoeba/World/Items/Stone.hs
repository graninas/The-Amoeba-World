module World.Items.Stone where

import World.World
import World.Player
import World.Geometry
import World.Stochastic
import World.Constants
import World.Types
import World.Id

import Data.Word
import System.Random
import qualified Data.List as L
import qualified Data.Either as E

data Stone = Stone { stoneId :: ItemId
                   , stoneOwner :: Player }
  deriving (Show, Read)

instance Id Stone where
    getId = stoneId

instance Active Stone where
    activate w p _ = inactive w p
    ownedBy = stoneOwner
    
instance Descripted Stone where
    description = show
    
stone :: ItemId -> Player -> Point -> [(Point, ActiveItems)]
stone sId pl p = [(p, packItems [Stone sId pl])]