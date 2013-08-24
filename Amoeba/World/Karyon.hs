module World.Karyon where

import Prelude hiding (Bounded)

import World.World
import World.Player
import World.Geometry

import qualified Data.List as L

data Energy = Energy Int

data Karyon = Karyon { karyonPlayer :: Player
                     , karyonEnergy :: Energy }
            | KaryonFiller { fillerRelativeShift :: Shift }


instance Active Karyon where
  activate k@(KaryonFiller _) = inactive k
  activate k@(Karyon _ _) = activateKaryon k
  
instance Bounded Karyon where
  bounds _ = \pos -> BoundCircle pos 10
  
karyon :: Player -> Int -> Point -> [(Point, Karyon)]
karyon pl e pos = kayronCell : fillers
  where
    kayronCell = (pos, Karyon pl (Energy e))
    fillers = map makePointedFiller ringSquareShifts
    makePointedFiller sh = (sh pos, KaryonFiller sh)
    

    
activateKaryon :: i -> Point -> Activators -> World -> Activators
activateKaryon k p acts w = undefined