module World.Items.Border where

import World.World
import World.Player
import World.Geometry
import World.Stochastic
import World.Constants

import Data.Word
import System.Random
import qualified Data.List as L
import qualified Data.Either as E


data Border = Border { borderId :: ItemId
                     , borderPlayer :: Player }

instance Id Border where
    getId = borderId

instance Active Border where
    activate w p _ = inactive w p
    ownedBy = borderPlayer
    
    
border :: ItemId -> Player -> Point -> [(Point, Items)]
border bId pl p = [(p, makeItems [Border bId pl])]