module World.Items.Border where

import World.Types
import World.World
import World.Player
import World.Geometry
import World.Stochastic
import World.Constants
import World.Descripted
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
    activate p i w = (w, [activationAnnotation p i])
    ownedBy = borderPlayer
    name _ = "Border"

instance Descripted Border where
    description = show
    
border :: ItemId -> Player -> Point -> [(Point, Border)]
border bId pl p = [(p, Border bId pl)]