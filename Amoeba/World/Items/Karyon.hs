module World.Items.Karyon where

import Prelude hiding (Bounded)

import World.Types
import World.World
import World.Player
import World.Geometry
import World.Stochastic
import World.Constants
import World.Items.Plasma
import World.Id

import System.Random
import qualified Data.List as L
import qualified Data.Either as E

karyonPieceActivateCount = 1

-- TODO: World bounds, conflicting cells, other deals

-- TODO: next plasma growing - with bounds, but with A* algorithm wich should
-- search the nearest possible cell for growing.

data Karyon = Karyon { karyonId :: ItemId
                     , karyonPlayer :: Player
                     , karyonEnergy :: Energy
                     , karyonFillers :: [Karyon]
                     , karyonBound :: Point -> Bound }
            | KaryonFiller { karyonId :: ItemId
                           , karyonPlayer :: Player
                           , karyonFillerShift :: Shift }

data SerializibleKaryon = SKaryon { sKaryonId :: ItemId
                                  , sKaryonPlayer :: Player
                                  , sKaryonEnergy :: Energy
                                  , sKaryonFillers :: [SerializibleKaryon]
                                  , sKaryonBound :: Bound }
                        | SKaryonFiller { sKaryonId :: ItemId
                                        , sKaryonPlayer :: Player
                                        , sKaryonFillerDir :: Direction }
  deriving (Show, Read)
    
mkSerializableKaryon (Karyon kId pl e fs b) = SKaryon kId pl e (map mkSerializableKaryon fs) (b zeroPoint)
mkSerializableKaryon (KaryonFiller kId pl sh) = SKaryonFiller kId pl (direction sh)

instance Id Karyon where
    getId = karyonId

instance Active Karyon where
    activate = activateKaryon
    ownedBy = karyonPlayer
    
instance Descripted Karyon where
    description  = show . mkSerializableKaryon
    
ordinalKaryonBound :: Point -> Bound
ordinalKaryonBound p = Circle p ordinalGrow

karyon :: ItemId -> Player -> Energy -> Point -> [(Point, Karyon)]
karyon kId pl e pos = kayronCell : pointedFillers
  where
    kayronCell = (pos, Karyon kId pl e fillers ordinalKaryonBound)
    pointedFillers = map makePointedFiller ringSquareShifts
    makePointedFiller sh = (sh pos, KaryonFiller kId pl sh)
    fillers = map snd pointedFillers

updateKaryonEnergyAnnotation :: Point -> Karyon -> Annotation
updateKaryonEnergyAnnotation p k@(Karyon _ pl e _ _) = annotation $ showPointAndPlayer p pl ++ " Karyon energy updated: " ++ show e
updateKaryonEnergyAnnotation _ _ = error "Not implemented"

updateKaryon :: Point -> Karyon -> (World, Annotations) -> (World, Annotations)
updateKaryon p k (w, anns) = let
    updIts = replaceItemFunc (p, k)
    ann = updateKaryonEnergyAnnotation p k
    in (w { worldMap = updateWorldMap [updIts] (worldMap w) }, anns ++ [ann])

activateKaryon :: Point -> Karyon -> World -> (World, Annotations)
activateKaryon p k@(KaryonFiller{}) w = inactive p k w
activateKaryon p k@(Karyon kId _ e fillers _) w = let
    shifts = map karyonFillerShift fillers
    f val = foldr (activatePiece p k) val shifts
    iteraties = iterate f (w, [], e)
    (w', anns, e') = head . drop karyonPieceActivateCount $ iteraties
    in updateKaryon p k { karyonEnergy = e' } (w', anns)

activatePiece :: Point -> Karyon -> Shift -> (World, Annotations, Energy) -> (World, Annotations, Energy)
activatePiece _ (KaryonFiller{}) _ r = r
activatePiece p k@(Karyon _ pl _ _ bound) sh (w, anns, e) = undefined -- TODO


