module World.Karyon where

import Prelude hiding (Bounded)

import World.World
import World.Player
import World.Geometry

import qualified Data.List as L

data Karyon = Karyon { player :: Player}

instance Active Karyon where
  activate k p w = undefined
  
  
  
karyon :: Player -> Point -> [(Point, Karyon)]
karyon pl pos = map makePointedKaryon (fullSquareFiller pos)
  where
    makePointedKaryon p = (p, Karyon pl)
    
    
    
    
-- Example


data Subkaryon = Subkaryon Point
  deriving (Show, Read, Eq)
instance Bounded Subkaryon where
    bounds (Subkaryon pos) = BoundCircle pos 10 
    