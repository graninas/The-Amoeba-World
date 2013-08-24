module World.Items.Stone where

import World.World
import World.Player
import World.Geometry
import World.Stochastic
import World.Constants

import Data.Word
import System.Random
import qualified Data.List as L
import qualified Data.Either as E


data Stone = Stone { stoneId :: ItemId
                   , stoneOwner :: Player }

instance Id Stone where
    getId = stoneId

instance Active Stone where
    activate w p _ = inactive w p
    ownedBy = stoneOwner
    
    
stone :: ItemId -> Player -> Point -> [(Point, Items)]
stone sId pl p = [(p, makeItems [Stone sId pl])]