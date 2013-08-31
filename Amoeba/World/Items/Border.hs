module World.Items.Border where

import World.Types
import World.World
import World.Player
import World.Geometry
import World.Stochastic
import World.Constants
import World.Id

import Data.Word
import System.Random
import qualified Data.List as L
import qualified Data.Either as E


data Border = Border { borderId :: ItemId
                     , borderPlayer :: Player }
  deriving (Show, Read, Eq)

instance Id Border where
    getId = borderId

instance Active Border where
    activate = inactive
    ownedBy = borderPlayer

instance Descripted Border where
    description = show
    
border :: ItemId -> Player -> Point -> [(Point, Border)]
border bId pl p = [(p, Border bId pl)]